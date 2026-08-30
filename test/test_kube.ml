module K = Kube

let widget_api =
  {
    K.Core.group = "testing.kube-ocaml.dev";
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

module Widget_api = K.Client.For (Widget)
module Widget_finalizer = K.Controller.Finalizer (Widget)

let test_paths () =
  let deployment =
    {
      K.Core.group = "apps";
      version = "v1";
      kind = "Deployment";
      plural = "deployments";
      scope = Namespaced;
    }
  in
  Alcotest.(check (result string string))
    "namespaced collection" (Ok "/apis/apps/v1/namespaces/team-a/deployments")
    (K.Core.collection_path deployment ~namespace:(Some "team-a"));
  Alcotest.(check (result string string))
    "all namespaces" (Ok "/apis/apps/v1/deployments")
    (K.Core.collection_path deployment ~namespace:None);
  let node =
    {
      K.Core.group = "";
      version = "v1";
      kind = "Node";
      plural = "nodes";
      scope = Cluster;
    }
  in
  Alcotest.(check (result string string))
    "cluster object" (Ok "/api/v1/nodes/worker-1")
    (K.Core.object_path node ~namespace:None ~name:"worker-1");
  Alcotest.(check bool)
    "reject namespace for cluster scope" true
    (Result.is_error (K.Core.collection_path node ~namespace:(Some "default")))

let kubeconfig_yaml =
  {|apiVersion: v1
kind: Config
clusters:
- cluster:
    certificate-authority-data: Q0E=
    server: https://127.0.0.1:6443
  name: local
contexts:
- context:
    cluster: local
    namespace: operators
    user: admin
  name: local
current-context: local
users:
- name: admin
  user:
    client-certificate-data: Q0VSVA==
    client-key-data: S0VZ
|}

let with_temp_file contents fn =
  let path = Filename.temp_file "kube-test-" ".yaml" in
  let channel = open_out_bin path in
  output_string channel contents;
  close_out channel;
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () -> fn path)

let write_file path contents =
  let channel = open_out_bin path in
  output_string channel contents;
  close_out channel

let with_temp_directory fn =
  let path = Filename.temp_dir "kube-test-" "" in
  let files = ref [] in
  let create name contents =
    let path = Filename.concat path name in
    write_file path contents;
    files := path :: !files;
    path
  in
  Fun.protect
    ~finally:(fun () ->
      List.iter Sys.remove !files;
      Unix.rmdir path)
    (fun () -> fn create)

let test_kubeconfig () =
  with_temp_file kubeconfig_yaml (fun path ->
      match K.Config.load_kubeconfig path with
      | Error message -> Alcotest.fail message
      | Ok config ->
          Alcotest.(check string)
            "server" "https://127.0.0.1:6443"
            (Uri.to_string config.server);
          Alcotest.(check (option string))
            "namespace" (Some "operators") config.namespace;
          Alcotest.(check (option string))
            "CA data" (Some "CA") config.tls.ca_pem;
          Alcotest.(check (option string))
            "client cert" (Some "CERT") config.tls.client_certificate_pem;
          Alcotest.(check (option string))
            "client key" (Some "KEY") config.tls.client_key_pem)

let test_merged_kubeconfigs () =
  with_temp_directory (fun create ->
      let ca_path = create "ca.pem" "MERGED CA" in
      let token_path = create "token" "merged-token\n" in
      let context =
        create "context.yaml"
          {|apiVersion: v1
kind: Config
current-context: merged
contexts:
- name: merged
  context:
    cluster: separate-cluster
    namespace: merged-ns
    user: separate-user
|}
      in
      let cluster =
        create "cluster.yaml"
          (Printf.sprintf
             {|apiVersion: v1
kind: Config
clusters:
- name: separate-cluster
  cluster:
    server: https://merged.example.invalid
    certificate-authority: %s
|}
             (Filename.basename ca_path))
      in
      let user =
        create "user.yaml"
          (Printf.sprintf
             {|apiVersion: v1
kind: Config
users:
- name: separate-user
  user:
    token: stale-inline-token
    tokenFile: %s
|}
             (Filename.basename token_path))
      in
      match K.Config.load_kubeconfigs [ context; cluster; user ] with
      | Error message -> Alcotest.fail message
      | Ok config -> (
          Alcotest.(check (option string))
            "merged namespace" (Some "merged-ns") config.namespace;
          Alcotest.(check (option string))
            "relative CA" (Some "MERGED CA") config.tls.ca_pem;
          match K.Config.authorization_header config with
          | Error message -> Alcotest.fail message
          | Ok value ->
              Alcotest.(check (option string))
                "relative token" (Some "Bearer merged-token") value))

let test_queue_deduplication () =
  let queue = K.Work_queue.create () in
  let key = K.Core.Object_key.make ~namespace:"default" "sample" in
  K.Work_queue.add queue key;
  K.Work_queue.add queue key;
  Alcotest.(check int) "one ready key" 1 (K.Work_queue.length queue);
  let taken = K.Work_queue.take queue in
  Alcotest.(check bool) "take key" true (taken = Some key);
  K.Work_queue.add queue key;
  K.Work_queue.add queue key;
  Alcotest.(check int) "dirty but not concurrent" 0 (K.Work_queue.length queue);
  K.Work_queue.task_done queue key;
  Alcotest.(check int)
    "queued once after completion" 1
    (K.Work_queue.length queue);
  ignore (K.Work_queue.take queue);
  K.Work_queue.task_done queue key;
  Alcotest.(check int)
    "empty after final completion" 0
    (K.Work_queue.length queue);
  K.Work_queue.close queue

let test_scheduler_shutdown () =
  let cancel = K.Cancel.create () in
  let queue = K.Work_queue.create () in
  let scheduler = K.Work_queue.Scheduler.create ~cancel queue in
  K.Work_queue.Scheduler.stop scheduler;
  K.Work_queue.Scheduler.stop scheduler;
  K.Work_queue.Scheduler.schedule scheduler ~after:0.0
    (K.Core.Object_key.make "after-stop");
  Alcotest.(check int)
    "stopped scheduler ignores new work" 0
    (K.Work_queue.length queue);
  K.Work_queue.close queue

let write_all fd value =
  let rec loop offset =
    if offset < String.length value then
      let count =
        Unix.write_substring fd value offset (String.length value - offset)
      in
      loop (offset + count)
  in
  loop 0

let read_request fd =
  let buffer = Bytes.create 4096 in
  let output = Buffer.create 4096 in
  let rec boundary value index =
    if index + 4 > String.length value then None
    else if String.sub value index 4 = "\r\n\r\n" then Some index
    else boundary value (index + 1)
  in
  let content_length head =
    String.split_on_char '\n' head
    |> List.find_map (fun line ->
        match String.split_on_char ':' line with
        | name :: values
          when String.lowercase_ascii (String.trim name) = "content-length" ->
            String.concat ":" values |> String.trim |> int_of_string_opt
        | _ -> None)
    |> Option.value ~default:0
  in
  let rec loop () =
    let contents = Buffer.contents output in
    match boundary contents 0 with
    | Some ending ->
        let required =
          ending + 4 + content_length (String.sub contents 0 ending)
        in
        if String.length contents >= required then
          String.sub contents 0 required
        else read_more ()
    | None -> read_more ()
  and read_more () =
    let count = Unix.read fd buffer 0 (Bytes.length buffer) in
    if count = 0 then Buffer.contents output
    else (
      Buffer.add_subbytes output buffer 0 count;
      loop ())
  in
  loop ()

let response ?(status = "200 OK") body =
  Printf.sprintf
    "HTTP/1.1 %s\r\n\
     Content-Type: application/json\r\n\
     Content-Length: %d\r\n\
     \r\n\
     %s"
    status (String.length body) body

let widget_json ?(namespace = "default") ~name ~resource_version () =
  `Assoc
    [
      ("apiVersion", `String "testing.kube-ocaml.dev/v1");
      ("kind", `String "Widget");
      ( "metadata",
        `Assoc
          [
            ("name", `String name);
            ("namespace", `String namespace);
            ("resourceVersion", `String resource_version);
          ] );
      ("spec", `Assoc []);
    ]

let list_json ?continue ?remaining ~resource_version items =
  let optional name fn = function
    | None -> []
    | Some value -> [ (name, fn value) ]
  in
  `Assoc
    [
      ("apiVersion", `String "testing.kube-ocaml.dev/v1");
      ("kind", `String "WidgetList");
      ( "metadata",
        `Assoc
          ([ ("resourceVersion", `String resource_version) ]
          @ optional "continue" (fun value -> `String value) continue
          @ optional "remainingItemCount" (fun value -> `Int value) remaining)
      );
      ("items", `List items);
    ]

let request_target request =
  match String.split_on_char '\n' request with
  | request_line :: _ -> (
      match String.split_on_char ' ' (String.trim request_line) with
      | _method :: target :: _ -> target
      | _ -> Alcotest.fail "invalid captured request line")
  | [] -> Alcotest.fail "empty captured request"

let request_body request =
  let marker = "\r\n\r\n" in
  let rec find index =
    if index + String.length marker > String.length request then None
    else if String.sub request index (String.length marker) = marker then
      Some (index + String.length marker)
    else find (index + 1)
  in
  match find 0 with
  | None -> Alcotest.fail "captured request has no header terminator"
  | Some start -> String.sub request start (String.length request - start)

let with_server replies fn =
  let listener = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> try Unix.close listener with Unix.Unix_error _ -> ())
    (fun () ->
      Unix.setsockopt listener Unix.SO_REUSEADDR true;
      Unix.bind listener (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
      Unix.listen listener (List.length replies);
      let port =
        match Unix.getsockname listener with
        | Unix.ADDR_INET (_, port) -> port
        | Unix.ADDR_UNIX _ -> assert false
      in
      let requests = ref [] in
      let server =
        Thread.create
          (fun () ->
            List.iter
              (fun reply ->
                let client, _ = Unix.accept listener in
                Fun.protect
                  ~finally:(fun () -> Unix.close client)
                  (fun () ->
                    requests := read_request client :: !requests;
                    write_all client reply))
              replies)
          ()
      in
      let config =
        {
          K.Config.server =
            Uri.of_string (Printf.sprintf "http://127.0.0.1:%d" port);
          namespace = None;
          credential = Anonymous;
          tls =
            {
              ca_pem = None;
              client_certificate_pem = None;
              client_key_pem = None;
              insecure_skip_verify = false;
              server_name = None;
            };
        }
      in
      let result = fn port config in
      Thread.join server;
      (result, List.rev !requests))

let test_streaming_http () =
  let listener = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> try Unix.close listener with Unix.Unix_error _ -> ())
    (fun () ->
      Unix.setsockopt listener Unix.SO_REUSEADDR true;
      Unix.bind listener (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
      Unix.listen listener 1;
      let port =
        match Unix.getsockname listener with
        | Unix.ADDR_INET (_, port) -> port
        | Unix.ADDR_UNIX _ -> assert false
      in
      let server =
        Thread.create
          (fun () ->
            let client, _ = Unix.accept listener in
            Fun.protect
              ~finally:(fun () -> Unix.close client)
              (fun () ->
                let buffer = Bytes.create 4096 in
                ignore (Unix.read client buffer 0 (Bytes.length buffer));
                write_all client
                  "HTTP/1.1 200 OK\r\n\
                   Transfer-Encoding: chunked\r\n\
                   \r\n\
                   5\r\n\
                   hello\r\n\
                   6\r\n\
                  \ world\r\n\
                   0\r\n\
                   \r\n"))
          ()
      in
      let config =
        {
          K.Config.server =
            Uri.of_string (Printf.sprintf "http://127.0.0.1:%d" port);
          namespace = None;
          credential = Anonymous;
          tls =
            {
              ca_pem = None;
              client_certificate_pem = None;
              client_key_pem = None;
              insecure_skip_verify = false;
              server_name = None;
            };
        }
      in
      let streamed = Buffer.create 16 in
      let response =
        K.Http.request
          ~on_chunk:(Buffer.add_string streamed)
          config `GET "/stream"
      in
      Thread.join server;
      match response with
      | Error message -> Alcotest.fail message
      | Ok response ->
          Alcotest.(check int) "status" 200 response.status;
          Alcotest.(check string)
            "streamed bytes" "hello world" (Buffer.contents streamed);
          Alcotest.(check string) "stream does not buffer body" "" response.body)

let test_http_body_limit () =
  let result, _requests =
    with_server
      [ response "12345" ]
      (fun _port config ->
        K.Http.request ~max_body_bytes:4 config `GET "/too-large")
  in
  match result with
  | Ok _ -> Alcotest.fail "oversized body was accepted"
  | Error message ->
      Alcotest.(check bool)
        "reports limit" true
        (String.starts_with ~prefix:"HTTP body exceeds 4 bytes" message)

let test_http_request_validation () =
  let config =
    {
      K.Config.server = Uri.of_string "http://127.0.0.1:1";
      namespace = None;
      credential = Anonymous;
      tls =
        {
          ca_pem = None;
          client_certificate_pem = None;
          client_key_pem = None;
          insecure_skip_verify = false;
          server_name = None;
        };
    }
  in
  Alcotest.(check bool)
    "reject unencoded whitespace" true
    (Result.is_error (K.Http.request config `GET "/bad path"));
  Alcotest.(check bool)
    "reject managed header" true
    (Result.is_error
       (K.Http.request ~headers:[ ("Content-Length", "99") ] config `GET "/"));
  Alcotest.(check bool)
    "reject invalid header name" true
    (Result.is_error
       (K.Http.request ~headers:[ ("Bad Header", "value") ] config `GET "/"))

let test_paginated_list () =
  let page_one =
    list_json ~continue:"next page" ~remaining:1 ~resource_version:"17"
      [ widget_json ~name:"first" ~resource_version:"11" () ]
    |> Yojson.Safe.to_string |> response
  in
  let page_two =
    list_json ~remaining:0 ~resource_version:"17"
      [ widget_json ~name:"second" ~resource_version:"12" () ]
    |> Yojson.Safe.to_string |> response
  in
  let result, requests =
    with_server [ page_one; page_two ] (fun _port config ->
        Widget_api.list_all ~page_size:1 (K.Client.create config))
  in
  (match result with
  | Error error -> Alcotest.fail (Format.asprintf "%a" K.Client.pp_error error)
  | Ok page ->
      Alcotest.(check int) "two aggregated items" 2 (List.length page.items);
      Alcotest.(check string)
        "snapshot version" "17"
        (K.Core.Resource_version.to_string page.resource_version));
  (match List.map request_target requests with
  | [ first; second ] ->
      let first = Uri.of_string first and second = Uri.of_string second in
      Alcotest.(check (option string))
        "first page limit" (Some "1")
        (Uri.get_query_param first "limit");
      Alcotest.(check (option string))
        "continuation token encoded and recovered" (Some "next page")
        (Uri.get_query_param second "continue")
  | _ -> Alcotest.fail "pagination did not make exactly two requests");
  let repeating_page =
    list_json ~continue:"same" ~resource_version:"17" []
    |> Yojson.Safe.to_string |> response
  in
  let result, _ =
    with_server [ repeating_page; repeating_page ] (fun _port config ->
        Widget_api.list_all ~page_size:1 (K.Client.create config))
  in
  Alcotest.(check bool)
    "repeated continuation token is rejected" true (Result.is_error result)

let test_watch_bookmark_and_expiry () =
  let event event_type object_json =
    `Assoc [ ("type", `String event_type); ("object", object_json) ]
    |> Yojson.Safe.to_string
  in
  let stream =
    String.concat "\n"
      [
        event "ADDED" (widget_json ~name:"watched" ~resource_version:"18" ());
        event "BOOKMARK"
          (`Assoc [ ("metadata", `Assoc [ ("resourceVersion", `String "19") ]) ]);
        "";
      ]
  in
  let seen = ref [] in
  let result, _ =
    with_server
      [ response stream ]
      (fun _port config ->
        Widget_api.watch (K.Client.create config)
          ~resource_version:(K.Core.Resource_version.of_string "17")
          ~on_event:(fun event -> seen := event :: !seen))
  in
  (match result with
  | Ok (K.Client.Watch_ended version) ->
      Alcotest.(check string)
        "bookmark advances watch" "19"
        (K.Core.Resource_version.to_string version)
  | Ok K.Client.Resource_version_expired ->
      Alcotest.fail "ordinary watch was treated as expired"
  | Error error -> Alcotest.fail (Format.asprintf "%a" K.Client.pp_error error));
  Alcotest.(check int) "added and bookmark delivered" 2 (List.length !seen);
  let gone =
    event "ERROR"
      (`Assoc
         [
           ("apiVersion", `String "v1");
           ("kind", `String "Status");
           ("reason", `String "Expired");
           ("message", `String "too old resource version");
           ("code", `Int 410);
         ])
    ^ "\n"
  in
  let result, _ =
    with_server
      [ response gone ]
      (fun _port config ->
        Widget_api.watch (K.Client.create config)
          ~resource_version:(K.Core.Resource_version.of_string "17")
          ~on_event:(fun _ -> ()))
  in
  Alcotest.(check bool)
    "in-band 410 requests relist" true
    (result = Ok K.Client.Resource_version_expired)

let test_patch_and_finalizer_requests () =
  let patched =
    widget_json ~name:"patched" ~resource_version:"21" ()
    |> Yojson.Safe.to_string |> response
  in
  let result, requests =
    with_server [ patched ] (fun _port config ->
        Widget_api.patch
          ~options:
            {
              K.Client.default_write_options with
              dry_run = [ "All" ];
              field_validation = Some `Strict;
            }
          (K.Client.create config) "patched"
          (K.Client.Apply
             {
               value = widget_json ~name:"patched" ~resource_version:"20" ();
               field_manager = "kube-ocaml-test";
               force = true;
             }))
  in
  (match result with
  | Ok _ -> ()
  | Error error -> Alcotest.fail (Format.asprintf "%a" K.Client.pp_error error));
  let request = List.hd requests in
  let target = request_target request |> Uri.of_string in
  Alcotest.(check (option string))
    "default object namespace"
    (Some "/apis/testing.kube-ocaml.dev/v1/namespaces/default/widgets/patched")
    (Some (Uri.path target));
  Alcotest.(check (option string))
    "apply manager" (Some "kube-ocaml-test")
    (Uri.get_query_param target "fieldManager");
  Alcotest.(check (option string))
    "apply force" (Some "true")
    (Uri.get_query_param target "force");
  Alcotest.(check bool)
    "apply content type" true
    (String.split_on_char '\n' request
    |> List.exists (fun line ->
        String.trim line = "Content-Type: application/apply-patch+yaml"));
  let without_version =
    match
      K.Dynamic.of_json (widget_json ~name:"patched" ~resource_version:"" ())
    with
    | Error message -> Alcotest.fail message
    | Ok value ->
        {
          value with
          metadata = { value.metadata with resource_version = None };
        }
  in
  let finalizer_response =
    widget_json ~name:"patched" ~resource_version:"22" ()
    |> Yojson.Safe.to_string |> response
  in
  let result, requests =
    with_server [ finalizer_response ] (fun _port config ->
        Widget_finalizer.ensure (K.Client.create config) without_version
          "testing.kube-ocaml.dev/finalizer")
  in
  (match result with
  | Ok _ -> ()
  | Error error -> Alcotest.fail (Format.asprintf "%a" K.Client.pp_error error));
  let body = List.hd requests |> request_body |> Yojson.Safe.from_string in
  let metadata =
    match body with
    | `Assoc fields -> List.assoc "metadata" fields
    | _ -> Alcotest.fail "finalizer patch is not a JSON object"
  in
  Alcotest.(check bool)
    "missing resource version is omitted, not null" false
    (match metadata with
    | `Assoc fields -> List.mem_assoc "resourceVersion" fields
    | _ -> Alcotest.fail "finalizer patch metadata is not an object")

let exec_kubeconfig ?(api_version = "client.authentication.k8s.io/v1")
    ?(command = "/usr/bin/printf") ?args ?(interactive_mode = Some "Never") port
    output =
  let args = Option.value ~default:[ "%s"; output ] args in
  let interactive_mode =
    match interactive_mode with
    | None -> []
    | Some value -> [ ("interactiveMode", `String value) ]
  in
  `Assoc
    [
      ("apiVersion", `String "v1");
      ("kind", `String "Config");
      ( "clusters",
        `List
          [
            `Assoc
              [
                ("name", `String "local");
                ( "cluster",
                  `Assoc
                    [
                      ( "server",
                        `String (Printf.sprintf "http://127.0.0.1:%d" port) );
                    ] );
              ];
          ] );
      ( "contexts",
        `List
          [
            `Assoc
              [
                ("name", `String "local");
                ( "context",
                  `Assoc
                    [ ("cluster", `String "local"); ("user", `String "exec") ]
                );
              ];
          ] );
      ("current-context", `String "local");
      ( "users",
        `List
          [
            `Assoc
              [
                ("name", `String "exec");
                ( "user",
                  `Assoc
                    [
                      ( "exec",
                        `Assoc
                          ([
                             ("apiVersion", `String api_version);
                             ("command", `String command);
                             ( "args",
                               `List
                                 (List.map (fun value -> `String value) args) );
                           ]
                          @ interactive_mode) );
                    ] );
              ];
          ] );
    ]

let test_exec_credential_refresh () =
  let unauthorized =
    response ~status:"401 Unauthorized"
      {|{"kind":"Status","apiVersion":"v1","status":"Failure","message":"expired","reason":"Unauthorized","code":401}|}
  in
  let result, requests =
    with_server
      [ unauthorized; response "{}" ]
      (fun port _config ->
        let credential =
          {|{"apiVersion":"client.authentication.k8s.io/v1","kind":"ExecCredential","status":{"expirationTimestamp":"2099-01-01T00:00:00Z","token":"exec-token"}}|}
        in
        with_temp_file
          (Yojson.Safe.to_string (exec_kubeconfig port credential))
          (fun path ->
            match K.Config.load_kubeconfig path with
            | Error message -> Error (K.Client.Invalid_request message)
            | Ok config -> K.Client.raw (K.Client.create config) `GET "/version"))
  in
  (match result with
  | Error error -> Alcotest.fail (Format.asprintf "%a" K.Client.pp_error error)
  | Ok response -> Alcotest.(check int) "retried response" 200 response.status);
  Alcotest.(check int) "one retry after 401" 2 (List.length requests);
  List.iter
    (fun request ->
      Alcotest.(check bool)
        "exec bearer header" true
        (String.split_on_char '\n' request
        |> List.exists (fun line ->
            String.trim line = "Authorization: Bearer exec-token")))
    requests

let test_exec_credential_contract () =
  let load ?api_version ?interactive_mode () =
    let document =
      exec_kubeconfig ?api_version ?interactive_mode 1 "unused"
      |> Yojson.Safe.to_string
    in
    with_temp_file document K.Config.load_kubeconfig
  in
  Alcotest.(check bool)
    "v1 requires interactiveMode" true
    (Result.is_error (load ~interactive_mode:None ()));
  Alcotest.(check bool)
    "v1beta1 defaults interactiveMode" true
    (Result.is_ok
       (load ~api_version:"client.authentication.k8s.io/v1beta1"
          ~interactive_mode:None ()));
  Alcotest.(check bool)
    "reject unsupported exec version" true
    (Result.is_error
       (load ~api_version:"client.authentication.k8s.io/v1alpha1" ()))

let test_relative_exec_credential () =
  with_temp_directory (fun create ->
      let credential =
        {|{"apiVersion":"client.authentication.k8s.io/v1","kind":"ExecCredential","status":{"token":"relative-token"}}|}
      in
      let helper =
        create "credential-helper"
          (Printf.sprintf "#!/bin/sh\nprintf '%%s' '%s'\n" credential)
      in
      Unix.chmod helper 0o700;
      let kubeconfig =
        exec_kubeconfig ~command:"./credential-helper" ~args:[] 1 credential
        |> Yojson.Safe.to_string |> create "config.json"
      in
      match K.Config.load_kubeconfig kubeconfig with
      | Error message -> Alcotest.fail message
      | Ok config -> (
          match K.Config.authorization_header config with
          | Error message -> Alcotest.fail message
          | Ok value ->
              Alcotest.(check (option string))
                "exec path is relative to kubeconfig"
                (Some "Bearer relative-token") value))

let test_cancel () =
  let cancel = K.Cancel.create () in
  let called = Atomic.make 0 in
  let _unregister = K.Cancel.on_cancel cancel (fun () -> Atomic.incr called) in
  K.Cancel.cancel cancel;
  K.Cancel.cancel cancel;
  Alcotest.(check int) "callback once" 1 (Atomic.get called);
  Alcotest.(check bool) "cancelled sleep" false (K.Cancel.sleep cancel 0.01)

let () =
  Alcotest.run "kube"
    [
      ("core", [ Alcotest.test_case "resource paths" `Quick test_paths ]);
      ( "config",
        [
          Alcotest.test_case "kind-style kubeconfig" `Quick test_kubeconfig;
          Alcotest.test_case "merged kubeconfigs" `Quick test_merged_kubeconfigs;
        ] );
      ( "runtime",
        [
          Alcotest.test_case "work queue deduplication" `Quick
            test_queue_deduplication;
          Alcotest.test_case "scheduler shutdown" `Quick test_scheduler_shutdown;
          Alcotest.test_case "cancellation" `Quick test_cancel;
        ] );
      ( "http",
        [
          Alcotest.test_case "chunked streaming" `Quick test_streaming_http;
          Alcotest.test_case "bounded body" `Quick test_http_body_limit;
          Alcotest.test_case "request validation" `Quick
            test_http_request_validation;
        ] );
      ( "client",
        [
          Alcotest.test_case "paginated list" `Quick test_paginated_list;
          Alcotest.test_case "watch bookmark and expiry" `Quick
            test_watch_bookmark_and_expiry;
          Alcotest.test_case "apply and finalizer requests" `Quick
            test_patch_and_finalizer_requests;
        ] );
      ( "auth",
        [
          Alcotest.test_case "exec credential refresh" `Quick
            test_exec_credential_refresh;
          Alcotest.test_case "exec credential contract" `Quick
            test_exec_credential_contract;
          Alcotest.test_case "relative exec credential" `Quick
            test_relative_exec_credential;
        ] );
    ]
