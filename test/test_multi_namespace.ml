module K = Kube
module T = Kube_test.Transport

let metadata namespace name revision =
  {
    K.Core.name;
    namespace = Some namespace;
    uid = None;
    resource_version =
      Some (K.Core.Resource_version.of_string (string_of_int revision));
    generation = None;
    deletion_timestamp = None;
    finalizers = [];
    owner_references = [];
    labels = [];
    annotations = [];
  }

module Entry = struct
  type t = { metadata : K.Core.object_meta; bucket : string; revision : int }

  let api =
    {
      K.Core.group = "testing.ocaml-kube.dev";
      version = "v1";
      kind = "Entry";
      plural = "entries";
      scope = Namespaced;
    }

  let metadata value = value.metadata
  let of_json _ = Error "not used by store tests"

  let to_json value =
    `Assoc
      [
        ("apiVersion", `String "testing.ocaml-kube.dev/v1");
        ("kind", `String "Entry");
        ("metadata", K.Core.object_meta_to_json value.metadata);
        ("bucket", `String value.bucket);
        ("revision", `Int value.revision);
      ]
end

module Entry_store = K.Store.Make (Entry)

let entry namespace name bucket revision =
  { Entry.metadata = metadata namespace name revision; bucket; revision }

let names values =
  List.map (fun (value : Entry.t) -> value.metadata.name) values
  |> List.sort String.compare

let require_store_ok = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let test_namespace_scoped_store_replacement () =
  let store = Entry_store.create () in
  ignore
    (Entry_store.replace_with_previous store
       [
         entry "alpha" "stable" "old" 1;
         entry "alpha" "obsolete" "old" 1;
         entry "beta" "preserved" "old" 1;
       ]);
  require_store_ok
    (Entry_store.add_index store ~name:"bucket" (fun value -> [ value.bucket ]));
  let replaced, removed =
    Entry_store.replace_namespace_with_previous store ~namespace:"alpha"
      [ entry "alpha" "stable" "new" 2; entry "alpha" "current" "new" 2 ]
    |> require_store_ok
  in
  (match replaced with
  | [ (Some previous, current); (None, added) ] ->
      Alcotest.(check int) "previous revision" 1 previous.revision;
      Alcotest.(check int) "current revision" 2 current.revision;
      Alcotest.(check string) "new object" "current" added.metadata.name
  | _ -> Alcotest.fail "unexpected scoped replacement transitions");
  Alcotest.(check (list string))
    "removed only alpha" [ "obsolete" ] (names removed);
  Alcotest.(check int) "combined cache length" 3 (Entry_store.length store);
  Alcotest.(check (list string))
    "old index keeps beta only" [ "preserved" ]
    (Entry_store.by_index store ~name:"bucket" "old"
    |> require_store_ok |> names);
  Alcotest.(check (list string))
    "new index contains alpha" [ "current"; "stable" ]
    (Entry_store.by_index store ~name:"bucket" "new"
    |> require_store_ok |> names);
  let before = Entry_store.items store |> names in
  Alcotest.(check bool)
    "mismatched namespace rejected" true
    (Result.is_error
       (Entry_store.replace_namespace_with_previous store ~namespace:"alpha"
          [ entry "beta" "wrong" "new" 3 ]));
  Alcotest.(check (list string))
    "failed replacement is atomic" before
    (Entry_store.items store |> names);
  let duplicate = entry "alpha" "duplicate" "new" 4 in
  Alcotest.(check bool)
    "duplicate keys rejected" true
    (Result.is_error
       (Entry_store.replace_namespace_with_previous store ~namespace:"alpha"
          [ duplicate; duplicate ]))

let test_large_store_stress () =
  let namespace_count = 10 in
  let per_namespace = 5_000 in
  let store = Entry_store.create () in
  let snapshot revision =
    List.init namespace_count (fun namespace_index ->
        let namespace = Printf.sprintf "ns-%02d" namespace_index in
        List.init per_namespace (fun index ->
            entry namespace
              (Printf.sprintf "item-%05d" index)
              (Printf.sprintf "bucket-%02d" (index mod 100))
              revision))
    |> List.flatten
  in
  ignore (Entry_store.replace_with_previous store (snapshot 1));
  require_store_ok
    (Entry_store.add_index store ~name:"bucket" (fun value -> [ value.bucket ]));
  Alcotest.(check int)
    "large initial cache"
    (namespace_count * per_namespace)
    (Entry_store.length store);
  let running = Atomic.make true in
  let failures = Atomic.make 0 in
  let readers =
    List.init 4 (fun reader ->
        Thread.create
          (fun () ->
            let random = Random.State.make [| 700 + reader |] in
            while Atomic.get running do
              let namespace =
                Printf.sprintf "ns-%02d"
                  (Random.State.int random namespace_count)
              in
              let name =
                Printf.sprintf "item-%05d"
                  (Random.State.int random per_namespace)
              in
              let key = K.Core.Object_key.make ~namespace name in
              if Entry_store.get store key = None then
                ignore (Atomic.fetch_and_add failures 1)
            done)
          ())
  in
  for revision = 2 to 21 do
    let namespace_index = revision mod namespace_count in
    let namespace = Printf.sprintf "ns-%02d" namespace_index in
    let values =
      List.init per_namespace (fun index ->
          entry namespace
            (Printf.sprintf "item-%05d" index)
            (Printf.sprintf "bucket-%02d" (index mod 100))
            revision)
    in
    if
      Result.is_error
        (Entry_store.replace_namespace_with_previous store ~namespace values)
    then ignore (Atomic.fetch_and_add failures 1)
  done;
  Atomic.set running false;
  List.iter Thread.join readers;
  Alcotest.(check int) "concurrent cache failures" 0 (Atomic.get failures);
  Alcotest.(check int)
    "large cache remains complete"
    (namespace_count * per_namespace)
    (Entry_store.length store);
  Alcotest.(check int)
    "large secondary index remains complete" (namespace_count * 50)
    (Entry_store.by_index store ~name:"bucket" "bucket-00"
    |> require_store_ok |> List.length)

let widget_api =
  {
    K.Core.group = "testing.ocaml-kube.dev";
    version = "v1";
    kind = "Widget";
    plural = "widgets";
    scope = Namespaced;
  }

module Widget = struct
  type t = K.Dynamic.t

  let api = widget_api
  let metadata value = value.K.Dynamic.metadata
  let of_json = K.Dynamic.of_json
  let to_json = K.Dynamic.to_json
end

module Widget_cache = K.Reflector.Make (Widget)
module Widget_controller = K.Controller.Make (Widget)

let gadget_api = { widget_api with kind = "Gadget"; plural = "gadgets" }

module Gadget = struct
  type t = K.Dynamic.t

  let api = gadget_api
  let metadata value = value.K.Dynamic.metadata
  let of_json = K.Dynamic.of_json
  let to_json = K.Dynamic.to_json
end

module Widget_owns_gadget = Widget_controller.Owns (Gadget)

module Cluster_entry = struct
  type t = Entry.t

  let api = { Entry.api with scope = K.Core.Cluster }
  let metadata = Entry.metadata
  let of_json = Entry.of_json
  let to_json = Entry.to_json
end

module Cluster_cache = K.Reflector.Make (Cluster_entry)

let rejects fn =
  try
    ignore (fn ());
    false
  with Invalid_argument _ -> true

let test_scope_validation () =
  Alcotest.(check bool)
    "scope forms are exclusive" true
    (rejects (fun () ->
         Widget_cache.create ~namespace:"alpha" ~namespaces:[ "beta" ] ()));
  Alcotest.(check bool)
    "empty selection rejected" true
    (rejects (fun () -> Widget_cache.create ~namespaces:[] ()));
  Alcotest.(check bool)
    "duplicate selection rejected" true
    (rejects (fun () -> Widget_cache.create ~namespaces:[ "alpha"; "alpha" ] ()));
  Alcotest.(check bool)
    "blank namespace rejected" true
    (rejects (fun () -> Widget_cache.create ~namespaces:[ " " ] ()));
  Alcotest.(check bool)
    "cluster selection rejected" true
    (rejects (fun () -> Cluster_cache.create ~namespaces:[ "alpha" ] ()));
  let supplied = Widget_cache.create ~namespaces:[ "alpha"; "beta" ] () in
  Alcotest.(check bool)
    "supplied cache owns controller scope" true
    (rejects (fun () ->
         Widget_controller.component ~cache:supplied ~namespace:"alpha"
           ~reconcile:(fun _ _ -> Ok K.Controller.Done)
           ()))

let widget_json ~namespace ~name ~resource_version =
  `Assoc
    [
      ("apiVersion", `String "testing.ocaml-kube.dev/v1");
      ("kind", `String "Widget");
      ( "metadata",
        `Assoc
          [
            ("name", `String name);
            ("namespace", `String namespace);
            ("resourceVersion", `String resource_version);
          ] );
    ]

let list_json resource_version items =
  `Assoc
    [
      ("apiVersion", `String "testing.ocaml-kube.dev/v1");
      ("kind", `String "WidgetList");
      ("metadata", `Assoc [ ("resourceVersion", `String resource_version) ]);
      ("items", `List items);
    ]

let expired_event =
  Yojson.Safe.to_string
    (`Assoc
       [
         ("type", `String "ERROR");
         ( "object",
           `Assoc
             [
               ("apiVersion", `String "v1");
               ("kind", `String "Status");
               ("reason", `String "Expired");
               ("message", `String "compacted");
               ("code", `Int 410);
             ] );
       ])
  ^ "\n"

let added_event value =
  Yojson.Safe.to_string
    (`Assoc [ ("type", `String "ADDED"); ("object", value) ])
  ^ "\n"

let request_namespace (request : T.request) =
  let segments =
    Uri.path (Uri.of_string request.target) |> String.split_on_char '/'
  in
  let rec find = function
    | "namespaces" :: namespace :: _ -> Some namespace
    | _ :: rest -> find rest
    | [] -> None
  in
  find segments

let is_watch (request : T.request) =
  Uri.get_query_param (Uri.of_string request.target) "watch" = Some "true"

let resource_version value =
  Option.map K.Core.Resource_version.to_string
    value.K.Dynamic.metadata.resource_version

let test_multi_namespace_reflector_recovery () =
  let lock = Mutex.create () in
  let list_counts = Hashtbl.create 3 in
  let watch_counts = Hashtbl.create 3 in
  let next table namespace =
    Mutex.lock lock;
    let count =
      Option.value ~default:0 (Hashtbl.find_opt table namespace) + 1
    in
    Hashtbl.replace table namespace count;
    Mutex.unlock lock;
    count
  in
  let transport =
    T.create (fun request ->
        let namespace =
          match request_namespace request with
          | Some namespace -> namespace
          | None ->
              Alcotest.fail
                "multi-namespace reflector used an all-namespace route"
        in
        if is_watch request then
          match (namespace, next watch_counts namespace) with
          | "alpha", 1 -> T.stream [ expired_event ]
          | _ -> T.stream ~wait_for_cancel:true []
        else
          match (namespace, next list_counts namespace) with
          | "alpha", 1 ->
              T.respond_json
                (list_json "10"
                   [
                     widget_json ~namespace ~name:"stable"
                       ~resource_version:"10";
                     widget_json ~namespace ~name:"obsolete"
                       ~resource_version:"10";
                   ])
          | "alpha", 2 ->
              T.respond_json
                (list_json "20"
                   [
                     widget_json ~namespace ~name:"stable"
                       ~resource_version:"20";
                     widget_json ~namespace ~name:"current"
                       ~resource_version:"20";
                   ])
          | "beta", 1 ->
              Unix.sleepf 0.05;
              T.respond_json
                (list_json "30"
                   [
                     widget_json ~namespace ~name:"preserved"
                       ~resource_version:"30";
                   ])
          | _ -> T.fail ("unexpected LIST for namespace " ^ namespace))
  in
  let client = Kube_test.client transport in
  let cancel = K.Cancel.create () in
  let cache = Widget_cache.create ~namespaces:[ "beta"; "alpha" ] () in
  ignore
    (Widget_cache.add_index cache ~name:"namespace" (fun value ->
         Option.to_list value.K.Dynamic.metadata.namespace)
    |> require_store_ok);
  let stable_transition = Atomic.make None in
  let obsolete_deleted = Atomic.make false in
  let unsubscribe =
    Widget_cache.subscribe cache (function
      | Widget_cache.Modified { previous = Some previous; current }
        when current.K.Dynamic.metadata.name = "stable"
             && current.metadata.namespace = Some "alpha" ->
          Atomic.set stable_transition
            (Some (resource_version previous, resource_version current))
      | Widget_cache.Deleted value
        when value.K.Dynamic.metadata.name = "obsolete" ->
          Atomic.set obsolete_deleted true
      | _ -> ())
  in
  let manager_result = Atomic.make None in
  let manager_thread =
    Thread.create
      (fun () ->
        let manager = K.Manager.create ~cancel client in
        K.Manager.add manager (Widget_cache.component cache);
        Atomic.set manager_result (Some (K.Manager.run manager)))
      ()
  in
  let wait_parent = K.Cancel.create () in
  let ready, timed_out =
    K.Cancel.with_timeout ~parent:wait_parent 3.0 (fun wait_cancel ->
        Widget_cache.await_ready ~cancel:wait_cancel cache)
  in
  Alcotest.(check bool) "readiness barrier completed" false timed_out;
  (match ready with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "cache readiness failed: %a" K.Client.pp_error error);
  let deadline = Unix.gettimeofday () +. 3.0 in
  let counts_ready () =
    Mutex.lock lock;
    let alpha =
      Option.value ~default:0 (Hashtbl.find_opt watch_counts "alpha")
    in
    let beta = Option.value ~default:0 (Hashtbl.find_opt watch_counts "beta") in
    Mutex.unlock lock;
    alpha >= 2 && beta >= 1
  in
  while (not (counts_ready ())) && Unix.gettimeofday () < deadline do
    Unix.sleepf 0.001
  done;
  if not (counts_ready ()) then Alcotest.fail "namespace watches did not start";
  K.Cancel.cancel cancel;
  Thread.join manager_thread;
  unsubscribe ();
  K.Client.close client;
  (match Atomic.get manager_result with
  | Some (Ok ()) -> ()
  | Some (Error error) ->
      Alcotest.failf "manager failed: %a" K.Manager.pp_error error
  | None -> Alcotest.fail "manager produced no result");
  Alcotest.(check bool)
    "cache stops after manager shutdown" false
    (Widget_cache.is_ready cache);
  let get namespace name =
    Widget_cache.get cache (K.Core.Object_key.make ~namespace name)
  in
  Alcotest.(check (option string))
    "alpha relist updated stable" (Some "20")
    (Option.bind (get "alpha" "stable") (fun value -> resource_version value));
  Alcotest.(check bool)
    "alpha relist removed obsolete" true
    (get "alpha" "obsolete" = None);
  Alcotest.(check bool)
    "alpha relist added current" true
    (Option.is_some (get "alpha" "current"));
  Alcotest.(check bool)
    "beta survived alpha relist" true
    (Option.is_some (get "beta" "preserved"));
  Alcotest.(check (option (pair (option string) (option string))))
    "alpha relist retained transition inputs"
    (Some (Some "10", Some "20"))
    (Atomic.get stable_transition);
  Alcotest.(check bool)
    "alpha deletion dispatched" true
    (Atomic.get obsolete_deleted);
  Alcotest.(check (list string))
    "alpha namespace index" [ "current"; "stable" ]
    (Widget_cache.by_index cache ~name:"namespace" "alpha"
    |> require_store_ok
    |> List.map (fun value -> value.K.Dynamic.metadata.name)
    |> List.sort String.compare);
  Alcotest.(check (list string))
    "beta namespace index" [ "preserved" ]
    (Widget_cache.by_index cache ~name:"namespace" "beta"
    |> require_store_ok
    |> List.map (fun value -> value.K.Dynamic.metadata.name));
  Alcotest.(check int)
    "alpha relisted once" 2
    (Hashtbl.find list_counts "alpha");
  Alcotest.(check int) "beta listed once" 1 (Hashtbl.find list_counts "beta")

let test_scope_mismatch_fails_reflector () =
  let transport =
    T.create (fun request ->
        let namespace =
          match request_namespace request with
          | Some namespace -> namespace
          | None -> Alcotest.fail "mismatch test used an all-namespace route"
        in
        if is_watch request then T.stream ~wait_for_cancel:true []
        else if namespace = "alpha" then
          T.respond_json
            (list_json "1"
               [
                 widget_json ~namespace:"beta" ~name:"wrong-scope"
                   ~resource_version:"1";
               ])
        else T.respond_json (list_json "1" []))
  in
  let client = Kube_test.client transport in
  let cache = Widget_cache.create ~namespaces:[ "alpha"; "beta" ] () in
  let manager = K.Manager.create client in
  K.Manager.add manager (Widget_cache.component cache);
  (match K.Manager.run manager with
  | Error { K.Manager.cause = K.Client.Decode message; _ } ->
      Alcotest.(check bool)
        "namespace mismatch identified" true
        (String.starts_with ~prefix:"invalid namespace-scoped LIST:" message)
  | Error error ->
      Alcotest.failf "unexpected manager error: %a" K.Manager.pp_error error
  | Ok () -> Alcotest.fail "cross-namespace LIST data was accepted");
  Alcotest.(check bool)
    "failed cache is not ready" false
    (Widget_cache.is_ready cache);
  K.Client.close client

let test_watch_scope_mismatch_fails_reflector () =
  let transport =
    T.create (fun request ->
        let namespace =
          match request_namespace request with
          | Some namespace -> namespace
          | None ->
              Alcotest.fail "watch mismatch test used an all-namespace route"
        in
        if not (is_watch request) then T.respond_json (list_json "1" [])
        else if namespace = "alpha" then
          T.stream
            [
              added_event
                (widget_json ~namespace:"beta" ~name:"wrong-watch-scope"
                   ~resource_version:"2");
            ]
        else T.stream ~wait_for_cancel:true [])
  in
  let client = Kube_test.client transport in
  let cache = Widget_cache.create ~namespaces:[ "alpha"; "beta" ] () in
  let manager = K.Manager.create client in
  K.Manager.add manager (Widget_cache.component cache);
  (match K.Manager.run manager with
  | Error { K.Manager.cause = K.Client.Decode message; _ } ->
      Alcotest.(check bool)
        "watch namespace mismatch identified" true
        (String.starts_with ~prefix:"watch object wrong-watch-scope" message)
  | Error error ->
      Alcotest.failf "unexpected manager error: %a" K.Manager.pp_error error
  | Ok () -> Alcotest.fail "cross-namespace watch event was accepted");
  let wait_cancel = K.Cancel.create () in
  (match Widget_cache.await_ready ~cancel:wait_cancel cache with
  | Error (K.Client.Decode message) ->
      Alcotest.(check bool)
        "terminal cache error is retained" true
        (String.starts_with ~prefix:"watch object wrong-watch-scope" message)
  | Error error ->
      Alcotest.failf "unexpected retained error: %a" K.Client.pp_error error
  | Ok () -> Alcotest.fail "failed cache remained ready");
  K.Client.close client

let gadget_json ~namespace ~name ~owner ~resource_version =
  `Assoc
    [
      ("apiVersion", `String "testing.ocaml-kube.dev/v1");
      ("kind", `String "Gadget");
      ( "metadata",
        `Assoc
          [
            ("name", `String name);
            ("namespace", `String namespace);
            ("resourceVersion", `String resource_version);
            ( "ownerReferences",
              `List
                [
                  `Assoc
                    [
                      ("apiVersion", `String "testing.ocaml-kube.dev/v1");
                      ("kind", `String "Widget");
                      ("name", `String owner);
                      ("uid", `String ("uid-" ^ owner));
                      ("controller", `Bool true);
                      ("blockOwnerDeletion", `Bool true);
                    ];
                ] );
          ] );
    ]

let resource_plural (request : T.request) =
  Uri.path (Uri.of_string request.target)
  |> String.split_on_char '/'
  |> List.filter (fun segment -> segment <> "")
  |> List.rev |> List.hd

let test_multi_namespace_owned_source () =
  let transport =
    T.create (fun request ->
        let namespace =
          match request_namespace request with
          | Some namespace -> namespace
          | None -> Alcotest.fail "owned source used an all-namespace route"
        in
        if is_watch request then T.stream ~wait_for_cancel:true []
        else
          match resource_plural request with
          | "widgets" -> T.respond_json (list_json "1" [])
          | "gadgets" ->
              T.respond_json
                (`Assoc
                   [
                     ("apiVersion", `String "testing.ocaml-kube.dev/v1");
                     ("kind", `String "GadgetList");
                     ("metadata", `Assoc [ ("resourceVersion", `String "1") ]);
                     ( "items",
                       `List
                         [
                           gadget_json ~namespace ~name:("child-" ^ namespace)
                             ~owner:("owner-" ^ namespace) ~resource_version:"1";
                         ] );
                   ])
          | resource -> T.fail ("unexpected resource " ^ resource))
  in
  let client = Kube_test.client transport in
  let owns = Widget_owns_gadget.make ~namespaces:[ "alpha"; "beta" ] () in
  let parent = K.Cancel.create () in
  let seen_lock = Mutex.create () in
  let seen = Hashtbl.create 3 in
  let result, timed_out =
    K.Cancel.with_timeout ~parent 3.0 (fun cancel ->
        Widget_controller.run ~cancel ~namespaces:[ "alpha"; "beta" ]
          ~watches:[ owns ] client ~reconcile:(fun _ request ->
            Mutex.lock seen_lock;
            Hashtbl.replace seen
              (K.Core.Object_key.to_string request.Widget_controller.key)
              ();
            let complete = Hashtbl.length seen = 2 in
            Mutex.unlock seen_lock;
            if complete then K.Cancel.cancel request.cancel;
            Ok K.Controller.Done))
  in
  Alcotest.(check bool) "owned controller completed" false timed_out;
  (match result with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "owned controller failed: %a" K.Client.pp_error error);
  Alcotest.(check bool)
    "alpha owner enqueued" true
    (Hashtbl.mem seen "alpha/owner-alpha");
  Alcotest.(check bool)
    "beta owner enqueued" true
    (Hashtbl.mem seen "beta/owner-beta");
  K.Client.close client

let test_multi_namespace_controller () =
  let transport =
    T.create (fun request ->
        let namespace =
          match request_namespace request with
          | Some namespace -> namespace
          | None -> Alcotest.fail "controller used an all-namespace route"
        in
        if is_watch request then T.stream ~wait_for_cancel:true []
        else
          T.respond_json
            (list_json "1"
               [
                 widget_json ~namespace ~name:("root-" ^ namespace)
                   ~resource_version:"1";
               ]))
  in
  let client = Kube_test.client transport in
  let parent = K.Cancel.create () in
  let seen_lock = Mutex.create () in
  let seen = Hashtbl.create 3 in
  let result, timed_out =
    K.Cancel.with_timeout ~parent 3.0 (fun cancel ->
        Widget_controller.run ~cancel ~namespaces:[ "alpha"; "beta" ] client
          ~reconcile:(fun _ request ->
            Mutex.lock seen_lock;
            Option.iter
              (fun namespace -> Hashtbl.replace seen namespace ())
              request.Widget_controller.key.namespace;
            let complete = Hashtbl.length seen = 2 in
            Mutex.unlock seen_lock;
            if complete then K.Cancel.cancel request.cancel;
            Ok K.Controller.Done))
  in
  Alcotest.(check bool) "controller completed before deadline" false timed_out;
  (match result with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "controller failed: %a" K.Client.pp_error error);
  Alcotest.(check bool) "alpha reconciled" true (Hashtbl.mem seen "alpha");
  Alcotest.(check bool) "beta reconciled" true (Hashtbl.mem seen "beta");
  K.Client.close client

let () =
  Alcotest.run "Multi-namespace caches"
    [
      ( "store",
        [
          Alcotest.test_case "atomic namespace replacement" `Quick
            test_namespace_scoped_store_replacement;
          Alcotest.test_case "large concurrent cache" `Slow
            test_large_store_stress;
        ] );
      ( "runtime",
        [
          Alcotest.test_case "scope validation" `Quick test_scope_validation;
          Alcotest.test_case "independent recovery" `Quick
            test_multi_namespace_reflector_recovery;
          Alcotest.test_case "scope mismatch failure" `Quick
            test_scope_mismatch_fails_reflector;
          Alcotest.test_case "watch scope mismatch failure" `Quick
            test_watch_scope_mismatch_fails_reflector;
          Alcotest.test_case "controller integration" `Quick
            test_multi_namespace_controller;
          Alcotest.test_case "owned source integration" `Quick
            test_multi_namespace_owned_source;
        ] );
    ]
