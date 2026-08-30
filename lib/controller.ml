type reconcile_result = Done | Requeue | Requeue_after of float

module Make (Resource : Core.Resource) = struct
  module Api = Client.For (Resource)
  module Cache = Store.Make (Resource)

  type request = { key : Core.Object_key.t; resource : Resource.t option }
  type startup = Waiting | Ready | Failed of Client.error

  let run ?cancel ?namespace ?label_selector ?(workers = 1) client ~reconcile =
    if workers < 1 then
      Error (Client.Invalid_request "workers must be at least one")
    else
      let owned_cancel, cancel =
        match cancel with
        | Some value -> (false, value)
        | None -> (true, Cancel.create ())
      in
      let cache = Cache.create () in
      let queue = Work_queue.create () in
      let scheduler = Work_queue.Scheduler.create ~cancel queue in
      let startup_mutex = Mutex.create () in
      let startup_changed = Condition.create () in
      let startup = ref Waiting in
      let fatal_mutex = Mutex.create () in
      let fatal_error = ref None in
      let set_startup value =
        Mutex.lock startup_mutex;
        startup := value;
        Condition.broadcast startup_changed;
        Mutex.unlock startup_mutex
      in
      let startup_is_waiting () =
        Mutex.lock startup_mutex;
        let result = !startup = Waiting in
        Mutex.unlock startup_mutex;
        result
      in
      let record_fatal error =
        Mutex.lock fatal_mutex;
        if !fatal_error = None then fatal_error := Some error;
        Mutex.unlock fatal_mutex;
        if startup_is_waiting () then set_startup (Failed error);
        Cancel.cancel cancel
      in
      let fatal () =
        Mutex.lock fatal_mutex;
        let result = !fatal_error in
        Mutex.unlock fatal_mutex;
        result
      in
      let backoff attempt =
        min 30.0 (0.25 *. (2. ** float_of_int (min attempt 7)))
      in
      let rec reflector_loop attempt resource_version =
        if not (Cancel.is_cancelled cancel) then
          match
            Api.watch ~cancel ?namespace ?label_selector client
              ~resource_version ~on_event:(function
              | Client.Added value | Client.Modified value ->
                  Cache.upsert cache value;
                  Work_queue.add queue
                    (Core.key_of_meta (Resource.metadata value))
              | Client.Deleted value ->
                  let key = Core.key_of_meta (Resource.metadata value) in
                  Cache.remove cache key;
                  Work_queue.add queue key
              | Client.Bookmark _ -> ()
              | Client.Watch_error error ->
                  Printf.eprintf "watch error %d: %s\n%!" error.code
                    error.message)
          with
          | Ok (Client.Watch_ended latest) -> reflector_loop 0 latest
          | Ok Client.Resource_version_expired -> relist 0
          | Error (Client.Transport _) when Cancel.is_cancelled cancel -> ()
          | Error error ->
              Format.eprintf "watch reconnect after %a@." Client.pp_error error;
              if Cancel.sleep cancel (backoff attempt) then
                reflector_loop (attempt + 1) resource_version
      and relist attempt =
        if not (Cancel.is_cancelled cancel) then
          match Api.list_all ~cancel ?namespace ?label_selector client with
          | Ok snapshot ->
              let removed = Cache.replace cache snapshot.items in
              List.iter
                (fun value ->
                  Work_queue.add queue
                    (Core.key_of_meta (Resource.metadata value)))
                snapshot.items;
              List.iter (Work_queue.add queue) removed;
              set_startup Ready;
              reflector_loop 0 snapshot.resource_version
          | Error (Client.Transport _) when Cancel.is_cancelled cancel -> ()
          | Error error when startup_is_waiting () -> set_startup (Failed error)
          | Error error ->
              Format.eprintf "list retry after %a@." Client.pp_error error;
              if Cancel.sleep cancel (backoff attempt) then relist (attempt + 1)
      in
      let reflector =
        Thread.create
          (fun () ->
            try relist 0
            with exn ->
              record_fatal
                (Client.Transport ("reflector failed: " ^ Printexc.to_string exn)))
          ()
      in
      let unregister =
        Cancel.on_cancel cancel (fun () ->
            Mutex.lock startup_mutex;
            Condition.broadcast startup_changed;
            Mutex.unlock startup_mutex;
            Work_queue.close queue)
      in
      Mutex.lock startup_mutex;
      while !startup = Waiting && not (Cancel.is_cancelled cancel) do
        Condition.wait startup_changed startup_mutex
      done;
      let initial = !startup in
      Mutex.unlock startup_mutex;
      match initial with
      | Failed error ->
          Cancel.cancel cancel;
          Thread.join reflector;
          Work_queue.close queue;
          Work_queue.Scheduler.stop scheduler;
          unregister ();
          Error error
      | Waiting ->
          Thread.join reflector;
          Work_queue.Scheduler.stop scheduler;
          unregister ();
          Ok ()
      | Ready -> (
          let retries = Hashtbl.create 127 in
          let retries_mutex = Mutex.create () in
          let clear_retry key =
            Mutex.lock retries_mutex;
            Hashtbl.remove retries key;
            Mutex.unlock retries_mutex
          in
          let next_retry key =
            Mutex.lock retries_mutex;
            let attempt =
              Option.value ~default:0 (Hashtbl.find_opt retries key)
            in
            Hashtbl.replace retries key (attempt + 1);
            Mutex.unlock retries_mutex;
            backoff attempt
          in
          let rec worker () =
            match Work_queue.take queue with
            | None -> ()
            | Some key ->
                let request = { key; resource = Cache.get cache key } in
                let result =
                  try reconcile client request
                  with exn ->
                    Error
                      ("uncaught reconciler exception: "
                     ^ Printexc.to_string exn)
                in
                (match result with
                | Ok Done -> clear_retry key
                | Ok Requeue ->
                    clear_retry key;
                    Work_queue.Scheduler.schedule scheduler ~after:0.0 key
                | Ok (Requeue_after delay) ->
                    clear_retry key;
                    Work_queue.Scheduler.schedule scheduler ~after:delay key
                | Error message ->
                    let delay = next_retry key in
                    Printf.eprintf "reconcile %s failed, retry in %.2fs: %s\n%!"
                      (Core.Object_key.to_string key)
                      delay message;
                    Work_queue.Scheduler.schedule scheduler ~after:delay key);
                Work_queue.task_done queue key;
                worker ()
          in
          let worker_threads =
            List.init workers (fun _ -> Thread.create worker ())
          in
          while not (Cancel.is_cancelled cancel) do
            ignore (Cancel.sleep cancel 1.0)
          done;
          Work_queue.close queue;
          Thread.join reflector;
          List.iter Thread.join worker_threads;
          Work_queue.Scheduler.stop scheduler;
          unregister ();
          if owned_cancel then Cancel.cancel cancel;
          match fatal () with
          | Some error -> Error error
          | None -> Ok ())
end

module Finalizer (Resource : Core.Resource) = struct
  module Api = Client.For (Resource)

  let update ?cancel client resource finalizers =
    let metadata = Resource.metadata resource in
    let resource_version =
      match metadata.resource_version with
      | None -> []
      | Some value ->
          [
            ("resourceVersion", `String (Core.Resource_version.to_string value));
          ]
    in
    let patch =
      `Assoc
        [
          ( "metadata",
            `Assoc
              (resource_version
              @ [
                  ( "finalizers",
                    `List (List.map (fun value -> `String value) finalizers) );
                ]) );
        ]
    in
    Api.patch ?cancel client ?namespace:metadata.namespace metadata.name
      (Client.Merge_patch patch)

  let ensure ?cancel client resource finalizer =
    let metadata = Resource.metadata resource in
    if List.mem finalizer metadata.finalizers then Ok resource
    else update ?cancel client resource (metadata.finalizers @ [ finalizer ])

  let remove ?cancel client resource finalizer =
    let metadata = Resource.metadata resource in
    if not (List.mem finalizer metadata.finalizers) then Ok resource
    else
      update ?cancel client resource
        (List.filter (fun value -> value <> finalizer) metadata.finalizers)
end
