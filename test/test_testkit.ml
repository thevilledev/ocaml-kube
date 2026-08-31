module K = Kube
module T = Kube_test.Transport

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

module Widgets = K.Client.For (Widget)
module Widget_controller = K.Controller.Make (Widget)

let require_ok label = function
  | Ok value -> value
  | Error error -> Alcotest.failf "%s: %a" label K.Client.pp_error error

let require_complete transport =
  match T.verify_complete transport with
  | Ok () -> ()
  | Error message -> Alcotest.fail message

let widget_json ?(resource_version = "1") name =
  `Assoc
    [
      ("apiVersion", `String "testing.ocaml-kube.dev/v1");
      ("kind", `String "Widget");
      ( "metadata",
        `Assoc
          [
            ("name", `String name);
            ("namespace", `String "default");
            ("resourceVersion", `String resource_version);
          ] );
    ]

let list_json items =
  `Assoc
    [
      ("apiVersion", `String "testing.ocaml-kube.dev/v1");
      ("kind", `String "WidgetList");
      ("metadata", `Assoc [ ("resourceVersion", `String "1") ]);
      ("items", `List items);
    ]

let test_authenticated_typed_request () =
  let transport = T.scripted [ T.respond_json (widget_json "alpha") ] in
  let configuration =
    Kube_test.config ~namespace:"default"
      ~credential:(K.Config.Static_token "test-secret") ()
  in
  let client = Kube_test.client ~configuration transport in
  let widget = require_ok "typed GET" (Widgets.get client "alpha") in
  Alcotest.(check string) "decoded name" "alpha" widget.metadata.name;
  (match T.requests transport with
  | [ request ] ->
      Alcotest.(check string)
        "request target"
        "/apis/testing.ocaml-kube.dev/v1/namespaces/default/widgets/alpha"
        request.target;
      Alcotest.(check (option string))
        "authorization header" (Some "Bearer test-secret")
        (T.header request "authorization")
  | requests ->
      Alcotest.failf "expected one request, received %d" (List.length requests));
  require_complete transport;
  K.Client.close client;
  Alcotest.(check bool) "close propagated" true (T.is_closed transport)

let test_typed_mutations () =
  let source =
    match Widget.of_json (widget_json "created") with
    | Ok value -> value
    | Error message -> Alcotest.fail message
  in
  let transport =
    T.scripted
      [
        T.respond_json (widget_json "created");
        T.respond_json ~status:200 (widget_json ~resource_version:"2" "created");
      ]
  in
  let client = Kube_test.client transport in
  ignore (require_ok "typed CREATE" (Widgets.create client source));
  ignore
    (require_ok "typed PATCH"
       (Widgets.patch client ~namespace:"default" "created"
          (K.Client.Merge_patch
             (`Assoc [ ("metadata", `Assoc [ ("labels", `Assoc []) ]) ]))));
  (match T.requests transport with
  | [ create; patch ] ->
      Alcotest.(check bool) "CREATE method" true (create.meth = `POST);
      Alcotest.(check bool) "PATCH method" true (patch.meth = `PATCH);
      Alcotest.(check (option string))
        "CREATE media type" (Some "application/json")
        (T.header create "content-type");
      Alcotest.(check (option string))
        "PATCH media type" (Some "application/merge-patch+json")
        (T.header patch "Content-Type");
      Alcotest.(check bool)
        "serialized create body" true
        (Option.is_some create.body);
      Alcotest.(check bool)
        "serialized patch body" true
        (Option.is_some patch.body)
  | requests ->
      Alcotest.failf "expected two mutations, received %d"
        (List.length requests));
  require_complete transport;
  K.Client.close client

let with_temp_file prefix suffix contents fn =
  let path = Filename.temp_file prefix suffix in
  let channel = open_out_bin path in
  output_string channel contents;
  close_out channel;
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)
    (fun () -> fn path)

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let test_exec_credential_retry () =
  with_temp_file "kube-test-count-" ".txt" "" (fun count_path ->
      let credential =
        {|{"apiVersion":"client.authentication.k8s.io/v1","kind":"ExecCredential","status":{"expirationTimestamp":"2099-01-01T00:00:00Z","token":"exec-secret"}}|}
      in
      let script =
        Printf.sprintf "#!/bin/sh\nprintf x >> '%s'\nprintf '%%s\\n' '%s'\n"
          count_path credential
      in
      with_temp_file "kube-test-credential-" ".sh" script (fun script_path ->
          Unix.chmod script_path 0o700;
          let kubeconfig =
            Printf.sprintf
              {|apiVersion: v1
kind: Config
clusters:
- name: test
  cluster:
    server: https://kubernetes.test
contexts:
- name: test
  context:
    cluster: test
    namespace: default
    user: test
current-context: test
users:
- name: test
  user:
    exec:
      apiVersion: client.authentication.k8s.io/v1
      command: "%s"
      interactiveMode: Never
|}
              script_path
          in
          with_temp_file "kube-test-config-" ".yaml" kubeconfig
            (fun kubeconfig_path ->
              let configuration =
                match K.Config.load_kubeconfig kubeconfig_path with
                | Ok value -> value
                | Error message -> Alcotest.fail message
              in
              let unauthorized =
                T.respond_json ~status:401
                  (`Assoc
                     [
                       ("apiVersion", `String "v1");
                       ("kind", `String "Status");
                       ("status", `String "Failure");
                       ("reason", `String "Unauthorized");
                       ("message", `String "expired credential");
                       ("code", `Int 401);
                     ])
              in
              let transport =
                T.scripted [ unauthorized; T.respond_json (`Assoc []) ]
              in
              let client = Kube_test.client ~configuration transport in
              ignore
                (require_ok "401 credential retry"
                   (K.Client.raw client `GET "/version"));
              Alcotest.(check int)
                "two wire attempts" 2
                (T.request_count transport);
              Alcotest.(check int)
                "credential helper rerun" 2
                (String.length (read_file count_path));
              List.iter
                (fun request ->
                  Alcotest.(check (option string))
                    "exec authorization" (Some "Bearer exec-secret")
                    (T.header request "Authorization"))
                (T.requests transport);
              require_complete transport;
              K.Client.close client)))

let test_bounds_streaming_and_close () =
  let transport =
    T.scripted [ T.respond "12345"; T.stream [ "first"; "-second" ] ]
  in
  let client = Kube_test.client transport in
  (match K.Client.raw ~max_body_bytes:4 client `GET "/bounded" with
  | Error (K.Client.Transport _) -> ()
  | Error error ->
      Alcotest.failf "unexpected bounded-body error: %a" K.Client.pp_error error
  | Ok _ -> Alcotest.fail "oversized buffered response succeeded");
  let chunks = Buffer.create 32 in
  ignore
    (require_ok "streamed response"
       (K.Client.raw ~max_body_bytes:1 ~on_chunk:(Buffer.add_string chunks)
          client `GET "/stream"));
  Alcotest.(check string)
    "chunks delivered without buffering" "first-second" (Buffer.contents chunks);
  require_complete transport;
  K.Client.close client;
  match K.Client.raw client `GET "/closed" with
  | Error (K.Client.Transport message) ->
      Alcotest.(check string)
        "closed error" "client transport is closed" message
  | Error error ->
      Alcotest.failf "unexpected close error: %a" K.Client.pp_error error
  | Ok _ -> Alcotest.fail "closed transport accepted a request"

let test_fragmented_watch () =
  let added =
    Yojson.Safe.to_string
      (`Assoc
         [
           ("type", `String "ADDED");
           ("object", widget_json ~resource_version:"2" "watched");
         ])
    ^ "\n"
  in
  let bookmark =
    Yojson.Safe.to_string
      (`Assoc
         [
           ("type", `String "BOOKMARK");
           ( "object",
             `Assoc
               [ ("metadata", `Assoc [ ("resourceVersion", `String "3") ]) ] );
         ])
    ^ "\n"
  in
  let payload = added ^ bookmark in
  let split = 13 in
  let transport =
    T.scripted
      [
        T.stream
          [
            String.sub payload 0 split;
            String.sub payload split (String.length payload - split);
          ];
      ]
  in
  let client = Kube_test.client transport in
  let events = ref [] in
  let result =
    Widgets.watch client ~namespace:"default"
      ~resource_version:(K.Core.Resource_version.of_string "1")
      ~on_event:(fun event -> events := event :: !events)
    |> require_ok "fragmented watch"
  in
  (match result with
  | K.Client.Watch_ended resource_version ->
      Alcotest.(check string)
        "latest resource version" "3"
        (K.Core.Resource_version.to_string resource_version)
  | K.Client.Resource_version_expired ->
      Alcotest.fail "ordinary watch was reported as expired");
  (match List.rev !events with
  | [ K.Client.Added widget; K.Client.Bookmark (Some resource_version) ] ->
      Alcotest.(check string) "watch object" "watched" widget.metadata.name;
      Alcotest.(check string)
        "bookmark" "3"
        (K.Core.Resource_version.to_string resource_version)
  | _ -> Alcotest.fail "unexpected decoded watch events");
  require_complete transport;
  K.Client.close client

let test_concurrent_capture () =
  let transport = T.create (fun _ -> T.respond_json (`Assoc [])) in
  let client = Kube_test.client transport in
  let failures = Atomic.make 0 in
  let threads =
    List.init 24 (fun index ->
        Thread.create
          (fun () ->
            match
              K.Client.raw client `GET ("/concurrent/" ^ string_of_int index)
            with
            | Ok _ -> ()
            | Error _ -> ignore (Atomic.fetch_and_add failures 1))
          ())
  in
  List.iter Thread.join threads;
  Alcotest.(check int) "concurrent failures" 0 (Atomic.get failures);
  Alcotest.(check int) "captured requests" 24 (T.request_count transport);
  K.Client.close client

let test_custom_transport_guards () =
  let calls = Atomic.make 0 in
  let closes = Atomic.make 0 in
  let transport =
    K.Client.Transport.make
      ~close:(fun () -> ignore (Atomic.fetch_and_add closes 1))
      (fun _ ->
        match Atomic.fetch_and_add calls 1 with
        | 0 ->
            Ok
              {
                K.Http.status = 200;
                reason = "OK";
                headers = [];
                body = "12345";
              }
        | _ -> failwith "injected failure")
  in
  let client =
    K.Client.create_with_transport ~transport (Kube_test.config ())
  in
  (match K.Client.raw ~max_body_bytes:4 client `GET "/oversized" with
  | Error (K.Client.Transport message) ->
      Alcotest.(check string)
        "central body guard" "HTTP body exceeds 4 bytes" message
  | Error error ->
      Alcotest.failf "unexpected body-guard error: %a" K.Client.pp_error error
  | Ok _ -> Alcotest.fail "custom transport bypassed the central body guard");
  (match K.Client.raw client `GET "/raise" with
  | Error (K.Client.Transport message) ->
      Alcotest.(check bool)
        "exception converted" true
        (String.starts_with ~prefix:"client transport raised:" message)
  | Error error ->
      Alcotest.failf "unexpected callback error: %a" K.Client.pp_error error
  | Ok _ -> Alcotest.fail "raising custom transport succeeded");
  let cancel = K.Cancel.create () in
  K.Cancel.cancel cancel;
  let cancelled_request : K.Client.Transport.request =
    {
      cancel = Some cancel;
      meth = `GET;
      target = "/cancelled";
      headers = [];
      body = None;
      on_chunk = None;
      max_body_bytes = None;
    }
  in
  (match K.Client.Transport.execute transport cancelled_request with
  | Error message ->
      Alcotest.(check string) "pre-cancel guard" "request cancelled" message
  | Ok _ -> Alcotest.fail "pre-cancelled custom request succeeded");
  Alcotest.(check int) "cancel avoided callback" 2 (Atomic.get calls);
  K.Client.close client;
  K.Client.close client;
  Alcotest.(check int) "close hook once" 1 (Atomic.get closes)

let is_watch target =
  Uri.get_query_param (Uri.of_string target) "watch" = Some "true"

let test_controller_over_injected_transport () =
  let transport =
    T.create (fun request ->
        if is_watch request.target then T.stream ~wait_for_cancel:true []
        else T.respond_json (list_json [ widget_json "controlled" ]))
  in
  let client = Kube_test.client transport in
  let parent = K.Cancel.create () in
  let health = K.Health.create () in
  let reconciliations = Atomic.make 0 in
  let check_unready label =
    match K.Health.readiness health with
    | Error [ failure ] ->
        Alcotest.(check string)
          (label ^ " check name") "controller/Widget/cache-sync" failure.check;
        Alcotest.(check string)
          (label ^ " message") "caches are not synchronized" failure.message
    | Error failures ->
        Alcotest.failf "%s: expected one readiness failure, received %d" label
          (List.length failures)
    | Ok () -> Alcotest.failf "%s: controller unexpectedly ready" label
  in
  let result, timed_out =
    K.Cancel.with_timeout ~parent 3.0 (fun cancel ->
        let manager = K.Manager.create ~cancel client in
        let controller =
          Widget_controller.component ~namespace:"default" ~health
            ~reconcile:(fun _ request ->
              (match K.Health.readiness health with
              | Ok () -> ()
              | Error failures ->
                  Alcotest.failf
                    "controller was unready during reconciliation: %a"
                    K.Health.pp_failures failures);
              (match request.resource with
              | Some widget ->
                  Alcotest.(check string)
                    "controller resource" "controlled" widget.metadata.name
              | None -> Alcotest.fail "initial object was absent from the cache");
              ignore (Atomic.fetch_and_add reconciliations 1);
              K.Cancel.cancel request.cancel;
              Ok K.Controller.Done)
            ()
        in
        check_unready "before component run";
        K.Manager.add manager controller;
        match K.Manager.run manager with
        | Ok () -> Ok ()
        | Error error -> Error error.K.Manager.cause)
  in
  Alcotest.(check bool) "controller did not time out" false timed_out;
  (match result with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "controller failed: %a" K.Client.pp_error error);
  Alcotest.(check int)
    "one level-triggered reconciliation" 1
    (Atomic.get reconciliations);
  check_unready "after controller shutdown";
  Alcotest.(check bool)
    "LIST passed through injected transport" true
    (List.exists
       (fun (request : T.request) -> not (is_watch request.target))
       (T.requests transport));
  K.Client.close client

let () =
  Alcotest.run "Kubernetes client testkit"
    [
      ( "transport",
        [
          Alcotest.test_case "authenticated typed request" `Quick
            test_authenticated_typed_request;
          Alcotest.test_case "typed mutations" `Quick test_typed_mutations;
          Alcotest.test_case "exec credential 401 retry" `Quick
            test_exec_credential_retry;
          Alcotest.test_case "bounds streaming and close" `Quick
            test_bounds_streaming_and_close;
          Alcotest.test_case "fragmented watch" `Quick test_fragmented_watch;
          Alcotest.test_case "concurrent capture" `Quick test_concurrent_capture;
          Alcotest.test_case "custom transport guards" `Quick
            test_custom_transport_guards;
          Alcotest.test_case "controller over injected transport" `Quick
            test_controller_over_injected_transport;
        ] );
    ]
