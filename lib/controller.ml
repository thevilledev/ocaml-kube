type reconcile_result = Done | Requeue | Requeue_after of float
type error_action = Retry_after of float | Drop

type error_policy =
  key:Core.Object_key.t -> attempt:int -> error:string -> error_action

let valid_delay value =
  value >= 0.0
  &&
  match classify_float value with
  | FP_normal | FP_subnormal | FP_zero -> true
  | FP_infinite | FP_nan -> false

let exponential_backoff ?(initial = 0.25) ?(maximum = 30.0) ?max_retries () =
  if (not (valid_delay initial)) || initial = 0.0 then
    invalid_arg
      "Controller.exponential_backoff: initial must be finite and positive";
  if (not (valid_delay maximum)) || maximum = 0.0 then
    invalid_arg
      "Controller.exponential_backoff: maximum must be finite and positive";
  if maximum < initial then
    invalid_arg
      "Controller.exponential_backoff: maximum must not be below initial";
  (match max_retries with
  | Some retries when retries < 0 ->
      invalid_arg
        "Controller.exponential_backoff: max_retries must not be negative"
  | _ -> ());
  fun ~key:_ ~attempt ~error:_ ->
    match max_retries with
    | Some retries when attempt >= retries -> Drop
    | _ ->
        Retry_after
          (min maximum (initial *. (2. ** float_of_int (min attempt 30))))

let default_error_policy = exponential_backoff ()

module Shared_reflector = Reflector

module Make (Resource : Core.Resource) = struct
  module Api = Client.For (Resource)
  module Cache = Shared_reflector.Make (Resource)
  module Cached = Cached_client.Make (Resource)

  type request = {
    key : Core.Object_key.t;
    resource : Resource.t option;
    reader : Cached.t;
    cancel : Cancel.t;
  }

  type source = {
    dependency : Manager.component;
    subscribe : Work_queue.t -> unit -> unit;
    await_ready : cancel:Cancel.t -> (unit, Client.error) result;
    seed : Work_queue.t -> unit;
  }

  type instrumentation = {
    active_workers : Metrics.Gauge.t;
    done_total : Metrics.Counter.t;
    requeue_total : Metrics.Counter.t;
    error_total : Metrics.Counter.t;
    timeout_total : Metrics.Counter.t;
    duration : Metrics.Histogram.t;
  }

  let value_of_event = function
    | Cache.Added value | Cache.Deleted value -> value
    | Cache.Modified { current; _ } -> current

  module Watches (Watched : Core.Resource) = struct
    module Cache = Shared_reflector.Make (Watched)

    let keys_of_event map = function
      | Cache.Added value | Cache.Deleted value -> map value
      | Cache.Modified { previous; current } ->
          Option.fold ~none:[] ~some:map previous @ map current
          |> List.sort_uniq Core.Object_key.compare

    let make ?cache ?namespace ?namespaces ?label_selector ?field_selector
        ?(predicate = fun _ -> true) ~map () =
      let cache =
        match cache with
        | Some _
          when namespace <> None || namespaces <> None || label_selector <> None
               || field_selector <> None ->
            invalid_arg
              "Controller.Watches.make: scope and selectors belong to the \
               supplied cache"
        | Some cache -> cache
        | None ->
            Cache.create ?namespace ?namespaces ?label_selector ?field_selector
              ()
      in
      let enqueue queue event =
        if predicate event then
          List.iter (Work_queue.add queue) (keys_of_event map event)
      in
      {
        dependency = Cache.component cache;
        subscribe = (fun queue -> Cache.subscribe cache (enqueue queue));
        await_ready = (fun ~cancel -> Cache.await_ready ~cancel cache);
        seed =
          (fun queue ->
            List.iter
              (fun value ->
                let event = Cache.Added value in
                if predicate event then
                  List.iter (Work_queue.add queue) (keys_of_event map event))
              (Cache.items cache));
      }
  end

  module Owns (Owned : Core.Resource) = struct
    module Watches = Watches (Owned)
    module Cache = Watches.Cache

    let owner_keys value =
      let metadata = Owned.metadata value in
      List.filter_map
        (fun reference ->
          if
            reference.Core.controller
            && reference.api_version = Core.api_version Resource.api
            && reference.kind = Resource.api.kind
          then
            Some
              (Core.Object_key.make
                 ?namespace:
                   (match Resource.api.scope with
                   | Core.Cluster -> None
                   | Core.Namespaced -> metadata.namespace)
                 reference.name)
          else None)
        metadata.owner_references

    let make ?cache ?namespace ?namespaces ?label_selector ?field_selector
        ?predicate () =
      Watches.make ?cache ?namespace ?namespaces ?label_selector ?field_selector
        ?predicate ~map:owner_keys ()
  end

  let instrumentation registry controller =
    let labels result = [ ("controller", controller); ("result", result) ] in
    let counter result =
      Metrics.Counter.create ~registry ~name:"ocaml_kube_reconciliations_total"
        ~help:"Reconciliation attempts by controller and result."
        ~labels:(labels result) ()
    in
    Some
      {
        active_workers =
          Metrics.Gauge.create ~registry
            ~name:"ocaml_kube_active_reconcile_workers"
            ~help:"Currently executing reconciliation workers."
            ~labels:[ ("controller", controller) ]
            ();
        done_total = counter "done";
        requeue_total = counter "requeue";
        error_total = counter "error";
        timeout_total = counter "timeout";
        duration =
          Metrics.Histogram.create ~registry
            ~name:"ocaml_kube_reconcile_duration_seconds"
            ~help:"Reconciliation callback duration in seconds."
            ~buckets:
              [ 0.005; 0.01; 0.025; 0.05; 0.1; 0.25; 0.5; 1.; 2.5; 5.; 10. ]
            ~labels:[ ("controller", controller) ]
            ();
      }

  let observe_result metrics started ~timed_out result =
    Option.iter
      (fun metrics ->
        Metrics.Gauge.dec metrics.active_workers;
        Metrics.Histogram.observe metrics.duration (Clock.elapsed started);
        Metrics.Counter.inc
          (if timed_out then metrics.timeout_total
           else
             match result with
             | Ok Done -> metrics.done_total
             | Ok Requeue | Ok (Requeue_after _) -> metrics.requeue_total
             | Error _ -> metrics.error_total))
      metrics

  let run_controller ~name ~cache ~watches ~predicate ~workers
      ~cache_sync_timeout ~reconcile_timeout ~metrics ~cache_ready ~error_policy
      ~reconcile ~client ~cancel =
    let set_cache_ready value =
      Option.iter (fun ready -> Atomic.set ready value) cache_ready
    in
    set_cache_ready false;
    if workers < 1 then
      Error (Client.Invalid_request "workers must be at least one")
    else
      let logger =
        Log.with_name (Client.logger client) "controller" |> fun logger ->
        Log.with_fields logger
          [
            ("controller", Log.String name);
            ("resource", Log.String Resource.api.plural);
          ]
      in
      let queue = Work_queue.create () in
      let scheduler = Work_queue.Scheduler.create ~cancel queue in
      let worker_threads = ref [] in
      let enqueue_primary event =
        if predicate event then
          Work_queue.add queue
            (Core.key_of_meta (Resource.metadata (value_of_event event)))
      in
      let unsubscribers =
        Cache.subscribe cache enqueue_primary
        :: List.map (fun source -> source.subscribe queue) watches
      in
      let unregister_cancel =
        Cancel.on_cancel cancel (fun () ->
            set_cache_ready false;
            Work_queue.close queue)
      in
      let cleanup () =
        set_cache_ready false;
        List.iter (fun unsubscribe -> unsubscribe ()) unsubscribers;
        Work_queue.close queue;
        Work_queue.Scheduler.stop scheduler;
        List.iter Thread.join !worker_threads;
        unregister_cancel ()
      in
      Fun.protect ~finally:cleanup (fun () ->
          let await_caches sync_cancel =
            let rec await_sources = function
              | [] -> Ok ()
              | source :: rest -> (
                  match source.await_ready ~cancel:sync_cancel with
                  | Ok () -> await_sources rest
                  | Error _ as error -> error)
            in
            match Cache.await_ready ~cancel:sync_cancel cache with
            | Error _ as error -> error
            | Ok () -> await_sources watches
          in
          let cache_result, cache_timed_out =
            Cancel.with_timeout ~parent:cancel cache_sync_timeout await_caches
          in
          if Cancel.is_cancelled cancel then Ok ()
          else if cache_timed_out then
            Error
              (Client.Transport
                 (Printf.sprintf "cache synchronization timed out after %.3fs"
                    cache_sync_timeout))
          else
            match cache_result with
            | Error _ as error -> error
            | Ok () ->
                let reader = Cached.make ~client ~cache in
                List.iter
                  (fun value -> enqueue_primary (Cache.Added value))
                  (Cache.items cache);
                List.iter (fun source -> source.seed queue) watches;
                Log.info logger
                  ~fields:[ ("workers", Log.Int workers) ]
                  "Controller caches synchronized";
                let retries = Hashtbl.create 127 in
                let retries_mutex = Mutex.create () in
                let clear_retry key =
                  Mutex.lock retries_mutex;
                  Hashtbl.remove retries key;
                  Mutex.unlock retries_mutex
                in
                let next_attempt key =
                  Mutex.lock retries_mutex;
                  let attempt =
                    Option.value ~default:0 (Hashtbl.find_opt retries key)
                  in
                  Hashtbl.replace retries key (attempt + 1);
                  Mutex.unlock retries_mutex;
                  attempt
                in
                let retry key message =
                  let attempt = next_attempt key in
                  let action =
                    try error_policy ~key ~attempt ~error:message
                    with exn ->
                      Log.error logger
                        ~fields:
                          [
                            ("key", Log.String (Core.Object_key.to_string key));
                            ("exception", Log.String (Printexc.to_string exn));
                          ]
                        "Controller error policy failed; using default";
                      default_error_policy ~key ~attempt ~error:message
                  in
                  match action with
                  | Drop ->
                      clear_retry key;
                      Log.error logger
                        ~fields:
                          [
                            ("key", Log.String (Core.Object_key.to_string key));
                            ("attempt", Log.Int (attempt + 1));
                            ("error", Log.String message);
                          ]
                        "Reconciliation failed and was dropped"
                  | Retry_after delay ->
                      let delay =
                        if valid_delay delay then delay
                        else
                          match
                            default_error_policy ~key ~attempt ~error:message
                          with
                          | Retry_after delay -> delay
                          | Drop -> assert false
                      in
                      Log.warn logger
                        ~fields:
                          [
                            ("key", Log.String (Core.Object_key.to_string key));
                            ("attempt", Log.Int (attempt + 1));
                            ("retry_after_seconds", Log.Float delay);
                            ("error", Log.String message);
                          ]
                        "Reconciliation failed; retrying";
                      Work_queue.Scheduler.schedule scheduler ~after:delay key
                in
                let rec worker () =
                  match Work_queue.take queue with
                  | None -> ()
                  | Some key ->
                      let resource = Cache.get cache key in
                      let started = Clock.now () in
                      Option.iter
                        (fun metrics ->
                          Metrics.Gauge.inc metrics.active_workers)
                        metrics;
                      let invoke request_cancel =
                        let request =
                          { key; resource; reader; cancel = request_cancel }
                        in
                        try reconcile client request
                        with exn ->
                          Error
                            ("uncaught reconciler exception: "
                           ^ Printexc.to_string exn)
                      in
                      let result, timed_out =
                        match reconcile_timeout with
                        | None -> (invoke cancel, false)
                        | Some timeout ->
                            let result, timed_out =
                              Cancel.with_timeout ~parent:cancel timeout invoke
                            in
                            if timed_out then
                              ( Error
                                  (Printf.sprintf
                                     "reconciliation timed out after %.3fs"
                                     timeout),
                                true )
                            else (result, false)
                      in
                      let result =
                        match result with
                        | Ok (Requeue_after delay) when not (valid_delay delay)
                          ->
                            Error
                              "reconciler returned a non-finite or negative \
                               requeue delay"
                        | result -> result
                      in
                      observe_result metrics started ~timed_out result;
                      (match result with
                      | Ok Done -> clear_retry key
                      | Ok Requeue ->
                          clear_retry key;
                          Work_queue.Scheduler.schedule scheduler ~after:0.0 key
                      | Ok (Requeue_after delay) ->
                          clear_retry key;
                          Work_queue.Scheduler.schedule scheduler ~after:delay
                            key
                      | Error message -> retry key message);
                      Work_queue.task_done queue key;
                      worker ()
                in
                for _ = 1 to workers do
                  worker_threads := Thread.create worker () :: !worker_threads
                done;
                if not (Cancel.is_cancelled cancel) then set_cache_ready true;
                if Cancel.is_cancelled cancel then set_cache_ready false;
                while not (Cancel.is_cancelled cancel) do
                  ignore (Cancel.sleep cancel 1.0)
                done;
                Ok ())

  let component ?cache ?namespace ?namespaces ?label_selector ?field_selector
      ?(watches = []) ?(predicate = fun _ -> true) ?name ?(workers = 1) ?metrics
      ?health ?(cache_sync_timeout = 120.) ?reconcile_timeout
      ?(error_policy = default_error_policy) ~reconcile () =
    if workers < 1 then
      invalid_arg "Controller.component: workers must be at least one";
    if (not (Float.is_finite cache_sync_timeout)) || cache_sync_timeout <= 0.
    then
      invalid_arg
        "Controller.component: cache_sync_timeout must be finite and positive";
    Option.iter
      (fun timeout ->
        if (not (Float.is_finite timeout)) || timeout <= 0. then
          invalid_arg
            "Controller.component: reconcile_timeout must be finite and \
             positive")
      reconcile_timeout;
    let cache =
      match cache with
      | Some _
        when namespace <> None || namespaces <> None || label_selector <> None
             || field_selector <> None ->
          invalid_arg
            "Controller.component: scope and selectors belong to the supplied \
             cache"
      | Some cache -> cache
      | None ->
          Cache.create ?namespace ?namespaces ?label_selector ?field_selector ()
    in
    let name = Option.value ~default:("controller/" ^ Resource.api.kind) name in
    if String.trim name = "" then
      invalid_arg "Controller.component: name must not be empty";
    let metrics =
      Option.bind metrics (fun registry -> instrumentation registry name)
    in
    let cache_ready =
      Option.map
        (fun health ->
          let ready = Atomic.make false in
          let _remove_cache_readiness =
            Health.add_readiness health ~name:(name ^ "/cache-sync") (fun () ->
                if Atomic.get ready then Ok ()
                else Error "caches are not synchronized")
          in
          ready)
        health
    in
    Manager.component ~name
      ~dependencies:
        (Cache.component cache
        :: List.map (fun source -> source.dependency) watches)
      (fun ~client ~cancel ->
        run_controller ~name ~cache ~watches ~predicate ~workers
          ~cache_sync_timeout ~reconcile_timeout ~metrics ~cache_ready
          ~error_policy ~reconcile ~client ~cancel)

  let run ?cancel ?cache ?namespace ?namespaces ?label_selector ?field_selector
      ?watches ?predicate ?name ?workers ?metrics ?health ?cache_sync_timeout
      ?reconcile_timeout ?error_policy client ~reconcile =
    let manager = Manager.create ?cancel client in
    Manager.add manager
      (component ?cache ?namespace ?namespaces ?label_selector ?field_selector
         ?watches ?predicate ?name ?workers ?metrics ?cache_sync_timeout ?health
         ?reconcile_timeout ?error_policy ~reconcile ());
    match Manager.run manager with
    | Ok () -> Ok ()
    | Error error -> Error error.Manager.cause
end

module Finalizer (Resource : Core.Resource) = struct
  module Api = Client.For (Resource)

  type event = Apply of Resource.t | Cleanup of Resource.t

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

  let error_string error = Format.asprintf "%a" Client.pp_error error

  let run ?cancel client resource finalizer reconcile =
    if String.trim finalizer = "" then Error "finalizer must not be empty"
    else
      let metadata = Resource.metadata resource in
      let present = List.mem finalizer metadata.finalizers in
      match (metadata.deletion_timestamp, present) with
      | None, false -> (
          match ensure ?cancel client resource finalizer with
          | Ok _ -> Ok Requeue
          | Error error -> Error (error_string error))
      | None, true -> reconcile (Apply resource)
      | Some _, false -> Ok Done
      | Some _, true -> (
          match reconcile (Cleanup resource) with
          | Error _ as error -> error
          | Ok ((Requeue | Requeue_after _) as result) -> Ok result
          | Ok Done -> (
              match remove ?cancel client resource finalizer with
              | Ok _ -> Ok Done
              | Error error -> Error (error_string error)))
end
