module Make (Resource : Core.Resource) = struct
  module Api = Client.For (Resource)
  module Cache = Store.Make (Resource)

  type event =
    | Added of Resource.t
    | Modified of { previous : Resource.t option; current : Resource.t }
    | Deleted of Resource.t

  type status = Idle | Running | Ready | Failed of Client.error | Stopped

  type scope = {
    namespace : string option;
    random : Random.State.t;
    mutable ready : bool;
  }

  type t = {
    scopes : scope list;
    label_selector : string option;
    field_selector : string option;
    cache : Cache.t;
    lock : Mutex.t;
    changed : Condition.t;
    dispatch_lock : Mutex.t;
    mutable status : status;
    mutable next_listener_id : int;
    mutable listeners : (int * (event -> unit)) list;
    mutable manager_component : Manager.component option;
  }

  let validate_namespace context namespace =
    if String.trim namespace = "" then
      invalid_arg (context ^ ": namespace must not be empty")

  let selected_namespaces ?namespace ?namespaces () =
    if Option.is_some namespace && Option.is_some namespaces then
      invalid_arg
        "Reflector.create: namespace and namespaces are mutually exclusive";
    match Resource.api.scope with
    | Core.Cluster ->
        if Option.is_some namespace || Option.is_some namespaces then
          invalid_arg
            "Reflector.create: namespaces are invalid for a cluster-scoped \
             resource";
        [ None ]
    | Core.Namespaced -> (
        match (namespace, namespaces) with
        | Some namespace, None ->
            validate_namespace "Reflector.create" namespace;
            [ Some namespace ]
        | None, Some [] ->
            invalid_arg "Reflector.create: namespaces must not be empty"
        | None, Some namespaces ->
            List.iter (validate_namespace "Reflector.create") namespaces;
            let sorted = List.sort String.compare namespaces in
            let unique = List.sort_uniq String.compare namespaces in
            if List.length sorted <> List.length unique then
              invalid_arg
                "Reflector.create: namespaces must not contain duplicates";
            List.map Option.some unique
        | None, None -> [ None ]
        | Some _, Some _ -> assert false)

  let create ?namespace ?namespaces ?label_selector ?field_selector () =
    let scopes =
      selected_namespaces ?namespace ?namespaces ()
      |> List.map (fun namespace ->
          { namespace; random = Random.State.make_self_init (); ready = false })
    in
    {
      scopes;
      label_selector;
      field_selector;
      cache = Cache.create ();
      lock = Mutex.create ();
      changed = Condition.create ();
      dispatch_lock = Mutex.create ();
      status = Idle;
      next_listener_id = 0;
      listeners = [];
      manager_component = None;
    }

  let protect reflector fn =
    Mutex.lock reflector.lock;
    Fun.protect ~finally:(fun () -> Mutex.unlock reflector.lock) fn

  let set_failed reflector error =
    protect reflector (fun () ->
        match reflector.status with
        | Failed _ -> ()
        | Idle | Running | Ready | Stopped ->
            reflector.status <- Failed error;
            Condition.broadcast reflector.changed)

  let mark_ready reflector scope =
    protect reflector (fun () ->
        if not scope.ready then scope.ready <- true;
        if
          List.for_all (fun scope -> scope.ready) reflector.scopes
          &&
          match reflector.status with
          | Failed _ -> false
          | _ -> true
        then reflector.status <- Ready;
        Condition.broadcast reflector.changed)

  let dispatch reflector logger event =
    let listeners =
      protect reflector (fun () -> List.map snd reflector.listeners)
    in
    Mutex.lock reflector.dispatch_lock;
    Fun.protect
      ~finally:(fun () -> Mutex.unlock reflector.dispatch_lock)
      (fun () ->
        List.iter
          (fun listener ->
            try listener event
            with exn ->
              Log.error logger
                ~fields:[ ("exception", Log.String (Printexc.to_string exn)) ]
                "Reflector listener failed")
          listeners)

  let subscribe reflector listener =
    let id =
      protect reflector (fun () ->
          let id = reflector.next_listener_id in
          reflector.next_listener_id <- id + 1;
          reflector.listeners <- (id, listener) :: reflector.listeners;
          id)
    in
    let subscribed = Atomic.make true in
    fun () ->
      if Atomic.compare_and_set subscribed true false then
        protect reflector (fun () ->
            reflector.listeners <-
              List.filter
                (fun (candidate, _) -> candidate <> id)
                reflector.listeners)

  let backoff scope attempt =
    let base = min 30.0 (0.25 *. (2. ** float_of_int (min attempt 7))) in
    base *. (0.8 +. Random.State.float scope.random 0.4)

  let retry_delay scope attempt error =
    let local = backoff scope attempt in
    match Client.Error.suggested_delay error with
    | None -> local
    | Some server -> max local server

  let logger_fields reflector =
    let api = Resource.api in
    [
      ("group", Log.String api.group);
      ("version", Log.String api.version);
      ("resource", Log.String api.plural);
    ]
    @
    match reflector.scopes with
    | [ { namespace = Some namespace; _ } ] ->
        [ ("namespace", Log.String namespace) ]
    | [ { namespace = None; _ } ] -> []
    | scopes -> [ ("namespaces", Log.Int (List.length scopes)) ]

  let scope_logger logger scope =
    match scope.namespace with
    | None -> logger
    | Some namespace ->
        Log.with_fields logger [ ("namespace", Log.String namespace) ]

  let validate_watch_value scope value =
    match scope.namespace with
    | None -> Ok ()
    | Some namespace ->
        let metadata = Resource.metadata value in
        if metadata.namespace = Some namespace then Ok ()
        else
          Error
            (Client.Decode
               (Printf.sprintf
                  "watch object %s belongs to namespace %s, expected %s"
                  metadata.name
                  (Option.value ~default:"<none>" metadata.namespace)
                  namespace))

  let install_snapshot reflector scope items =
    match scope.namespace with
    | None -> Ok (Cache.replace_with_previous reflector.cache items)
    | Some namespace -> (
        match
          Cache.replace_namespace_with_previous reflector.cache ~namespace items
        with
        | Ok value -> Ok value
        | Error message ->
            Error (Client.Decode ("invalid namespace-scoped LIST: " ^ message)))

  let run_scope reflector scope ~client ~cancel ~logger =
    let logger = scope_logger logger scope in
    let rec watch attempt resource_version =
      if Cancel.is_cancelled cancel then Ok ()
      else
        let started = Clock.now () in
        let latest = ref resource_version in
        let event_error = ref None in
        let observe_resource_version value =
          match (Resource.metadata value).resource_version with
          | None -> ()
          | Some resource_version -> latest := resource_version
        in
        let checked value fn =
          match validate_watch_value scope value with
          | Ok () -> fn value
          | Error error ->
              event_error := Some error;
              raise Exit
        in
        let result =
          Api.watch ~cancel ?namespace:scope.namespace
            ?label_selector:reflector.label_selector client ~resource_version
            ?field_selector:reflector.field_selector ~on_event:(function
            | Client.Added value ->
                checked value (fun value ->
                    observe_resource_version value;
                    ignore (Cache.upsert reflector.cache value);
                    dispatch reflector logger (Added value))
            | Client.Modified value ->
                checked value (fun value ->
                    observe_resource_version value;
                    let previous = Cache.upsert reflector.cache value in
                    dispatch reflector logger
                      (Modified { previous; current = value }))
            | Client.Deleted value ->
                checked value (fun value ->
                    observe_resource_version value;
                    let key = Core.key_of_meta (Resource.metadata value) in
                    ignore (Cache.remove reflector.cache key);
                    dispatch reflector logger (Deleted value))
            | Client.Bookmark (Some resource_version) ->
                latest := resource_version
            | Client.Bookmark None -> ()
            | Client.Watch_error error ->
                Log.warn logger
                  ~fields:
                    [
                      ("status_code", Log.Int error.code);
                      ("error", Log.String error.message);
                    ]
                  "Watch returned a Status error")
        in
        match !event_error with
        | Some error -> Error error
        | None -> (
            match result with
            | Ok (Client.Watch_ended latest) ->
                if Clock.elapsed started >= 1.0 then watch 0 latest
                else if Cancel.sleep cancel (backoff scope attempt) then
                  watch (attempt + 1) latest
                else Ok ()
            | Ok Client.Resource_version_expired -> relist 0
            | Error (Client.Transport _) when Cancel.is_cancelled cancel ->
                Ok ()
            | Error error ->
                let delay = retry_delay scope attempt error in
                Log.warn logger
                  ~fields:
                    [
                      ("attempt", Log.Int (attempt + 1));
                      ("retry_after_seconds", Log.Float delay);
                      ( "error",
                        Log.String (Format.asprintf "%a" Client.pp_error error)
                      );
                    ]
                  "Watch failed; reconnecting";
                if Cancel.sleep cancel delay then watch (attempt + 1) !latest
                else Ok ())
    and relist attempt =
      if Cancel.is_cancelled cancel then Ok ()
      else
        match
          Api.list_all ~cancel ?namespace:scope.namespace
            ?label_selector:reflector.label_selector
            ?field_selector:reflector.field_selector client
        with
        | Ok snapshot -> (
            match install_snapshot reflector scope snapshot.items with
            | Error _ as error -> error
            | Ok (replaced, removed) ->
                List.iter
                  (function
                    | None, current -> dispatch reflector logger (Added current)
                    | Some previous, current ->
                        dispatch reflector logger
                          (Modified { previous = Some previous; current }))
                  replaced;
                List.iter
                  (fun value -> dispatch reflector logger (Deleted value))
                  removed;
                mark_ready reflector scope;
                Log.info logger
                  ~fields:
                    [
                      ("scope_items", Log.Int (List.length snapshot.items));
                      ("cache_items", Log.Int (Cache.length reflector.cache));
                      ( "resource_version",
                        Log.String
                          (Core.Resource_version.to_string
                             snapshot.resource_version) );
                    ]
                  "Reflector cache scope synchronized";
                watch 0 snapshot.resource_version)
        | Error (Client.Transport _) when Cancel.is_cancelled cancel -> Ok ()
        | Error ((Client.Decode _ | Client.Invalid_request _) as error) ->
            Error error
        | Error error ->
            let delay = retry_delay scope attempt error in
            Log.warn logger
              ~fields:
                [
                  ("attempt", Log.Int (attempt + 1));
                  ("retry_after_seconds", Log.Float delay);
                  ( "error",
                    Log.String (Format.asprintf "%a" Client.pp_error error) );
                ]
              "Initial LIST failed; retrying";
            if Cancel.sleep cancel delay then relist (attempt + 1) else Ok ()
    in
    try relist 0
    with exn ->
      Error (Client.Transport ("reflector failed: " ^ Printexc.to_string exn))

  let run reflector ~client ~cancel =
    let logger =
      Log.with_name (Client.logger client) "reflector" |> fun logger ->
      Log.with_fields logger (logger_fields reflector)
    in
    let started =
      protect reflector (fun () ->
          match reflector.status with
          | Idle ->
              reflector.status <- Running;
              Condition.broadcast reflector.changed;
              true
          | Running | Ready | Failed _ | Stopped -> false)
    in
    if not started then
      Error (Client.Invalid_request "reflector can only be started once")
    else
      let () =
        Log.debug logger
          ~fields:[ ("scopes", Log.Int (List.length reflector.scopes)) ]
          "Reflector started"
      in
      let scope_cancel = Cancel.create () in
      let unlink =
        Cancel.on_cancel cancel (fun () -> Cancel.cancel scope_cancel)
      in
      let first_error = Atomic.make None in
      let threads = ref [] in
      let record_result scope result =
        match result with
        | Ok () -> ()
        | Error error ->
            if Atomic.compare_and_set first_error None (Some error) then
              set_failed reflector error;
            Log.error
              (scope_logger logger scope)
              ~fields:
                [
                  ( "error",
                    Log.String (Format.asprintf "%a" Client.pp_error error) );
                ]
              "Reflector scope failed";
            Cancel.cancel scope_cancel
      in
      let result =
        Fun.protect
          ~finally:(fun () -> unlink ())
          (fun () ->
            try
              List.iter
                (fun scope ->
                  let thread =
                    Thread.create
                      (fun () ->
                        run_scope reflector scope ~client ~cancel:scope_cancel
                          ~logger
                        |> record_result scope)
                      ()
                  in
                  threads := thread :: !threads)
                reflector.scopes;
              List.iter Thread.join !threads;
              match Atomic.get first_error with
              | Some error -> Error error
              | None -> Ok ()
            with exn ->
              Cancel.cancel scope_cancel;
              List.iter Thread.join !threads;
              Error
                (Client.Transport
                   ("failed to supervise reflector scopes: "
                  ^ Printexc.to_string exn)))
      in
      (match result with
      | Error error -> set_failed reflector error
      | Ok () ->
          protect reflector (fun () ->
              match reflector.status with
              | Failed _ -> ()
              | Idle | Running | Ready | Stopped ->
                  reflector.status <- Stopped;
                  Condition.broadcast reflector.changed));
      Log.debug logger
        ~fields:
          [
            ( "result",
              Log.String
                (match result with
                | Ok () -> "stopped"
                | Error _ -> "failed") );
          ]
        "Reflector stopped";
      result

  let component_name reflector =
    let api = Resource.api in
    let resource =
      String.concat "/"
        (List.filter (( <> ) "") [ api.group; api.version; api.plural ])
    in
    match reflector.scopes with
    | [ { namespace = None; _ } ] -> "reflector/" ^ resource
    | [ { namespace = Some namespace; _ } ] ->
        "reflector/" ^ resource ^ "/" ^ namespace
    | scopes ->
        let namespaces =
          List.filter_map (fun scope -> scope.namespace) scopes
          |> String.concat ","
        in
        "reflector/" ^ resource ^ "/namespaces/" ^ namespaces

  let component reflector =
    protect reflector (fun () ->
        match reflector.manager_component with
        | Some component -> component
        | None ->
            let component =
              Manager.component ~name:(component_name reflector)
                (fun ~client ~cancel -> run reflector ~client ~cancel)
            in
            reflector.manager_component <- Some component;
            component)

  let await_ready ~cancel reflector =
    let unregister =
      Cancel.on_cancel cancel (fun () ->
          protect reflector (fun () -> Condition.broadcast reflector.changed))
    in
    let result =
      protect reflector (fun () ->
          while
            (reflector.status = Idle || reflector.status = Running)
            && not (Cancel.is_cancelled cancel)
          do
            Condition.wait reflector.changed reflector.lock
          done;
          match reflector.status with
          | Ready -> Ok ()
          | Failed error -> Error error
          | Idle | Running | Stopped ->
              Error (Client.Transport "reflector stopped before cache sync"))
    in
    unregister ();
    result

  let is_ready reflector =
    protect reflector (fun () -> reflector.status = Ready)

  let get reflector = Cache.get reflector.cache
  let add_index reflector = Cache.add_index reflector.cache
  let by_index reflector = Cache.by_index reflector.cache
  let items reflector = Cache.items reflector.cache
  let length reflector = Cache.length reflector.cache
end
