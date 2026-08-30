module K = Kube
module K8s = Kube_api_v1_36

let fail message =
  Printf.eprintf "%s\n%!" message;
  exit 1

let require context = function
  | Ok value -> value
  | Error error ->
      fail (context ^ ": " ^ Format.asprintf "%a" K.Client.pp_error error)

let dynamic context value =
  match K.Dynamic.of_json value with
  | Ok resource -> resource
  | Error message -> fail (context ^ ": " ^ message)

let metadata name labels =
  `Assoc
    [
      ("name", `String name);
      ("namespace", `String "default");
      ( "labels",
        `Assoc (List.map (fun (key, value) -> (key, `String value)) labels) );
    ]

let config_map name =
  dynamic "construct ConfigMap"
    (`Assoc
       [
         ("apiVersion", `String "v1");
         ("kind", `String "ConfigMap");
         ("metadata", metadata name [ ("ocaml-k8s.dev/client-check", "true") ]);
         ("data", `Assoc [ ("proof", `String name) ]);
       ])

let deployment name =
  let labels = [ ("app", name) ] in
  dynamic "construct Deployment"
    (`Assoc
       [
         ("apiVersion", `String "apps/v1");
         ("kind", `String "Deployment");
         ("metadata", metadata name labels);
         ( "spec",
           `Assoc
             [
               ("replicas", `Int 0);
               ( "selector",
                 `Assoc
                   [
                     ( "matchLabels",
                       `Assoc
                         (List.map
                            (fun (key, value) -> (key, `String value))
                            labels) );
                   ] );
               ( "template",
                 `Assoc
                   [
                     ( "metadata",
                       `Assoc
                         [
                           ( "labels",
                             `Assoc
                               (List.map
                                  (fun (key, value) -> (key, `String value))
                                  labels) );
                         ] );
                     ( "spec",
                       `Assoc
                         [
                           ( "containers",
                             `List
                               [
                                 `Assoc
                                   [
                                     ("name", `String "proof");
                                     ( "image",
                                       `String "registry.k8s.io/pause:3.10" );
                                   ];
                               ] );
                         ] );
                   ] );
             ] );
       ])

let token_request =
  `Assoc
    [
      ("apiVersion", `String "authentication.k8s.io/v1");
      ("kind", `String "TokenRequest");
      ( "spec",
        `Assoc
          [
            ("audiences", `List [ `String "https://kubernetes.default.svc" ]);
            ("expirationSeconds", `Int 600);
          ] );
    ]

let has_token = function
  | `Assoc fields -> (
      match List.assoc_opt "status" fields with
      | Some (`Assoc status) -> (
          match List.assoc_opt "token" status with
          | Some (`String value) -> String.trim value <> ""
          | _ -> false)
      | _ -> false)
  | _ -> false

let rec replace_scale_after_fresh_get client deployment_name attempts =
  let scale =
    K.Dynamic.get_scale client ~api:K8s.Apps_v1.Deployment.api
      ~namespace:"default" deployment_name
    |> require "get Deployment Scale"
  in
  if scale.spec.replicas <> 0l then fail "new Deployment Scale is not zero";
  let scale = { scale with spec = { K.Client.replicas = 0l } } in
  match
    K.Dynamic.replace_scale client ~api:K8s.Apps_v1.Deployment.api
      ~namespace:"default" deployment_name scale
  with
  | Ok scale -> scale
  | Error error when K.Client.Error.is_conflict error && attempts > 0 ->
      Unix.sleepf 0.05;
      replace_scale_after_fresh_get client deployment_name (attempts - 1)
  | Error error ->
      fail
        ("replace Deployment Scale: "
        ^ Format.asprintf "%a" K.Client.pp_error error)

let () =
  let kubeconfig = ref None in
  let context = ref None in
  let set option value = option := Some value in
  Arg.parse
    [
      ( "--kubeconfig",
        Arg.String (set kubeconfig),
        "PATH Kubernetes kubeconfig path" );
      ("--context", Arg.String (set context), "NAME Kubeconfig context");
    ]
    (fun value -> raise (Arg.Bad ("unexpected argument: " ^ value)))
    "client-features-check [OPTIONS]";
  let config =
    match !kubeconfig with
    | Some path -> K.Config.load_kubeconfig ?context:!context path
    | None -> K.Config.load_default ?context:!context ()
  in
  let config =
    match config with
    | Ok config -> config
    | Error message -> fail ("configuration error: " ^ message)
  in
  let client = K.Client.create config in
  let deployment_name = "ocaml-k8s-client-check" in
  let selector = "ocaml-k8s.dev/client-check=true" in
  let cleanup () =
    ignore
      (K.Dynamic.delete client ~api:K8s.Apps_v1.Deployment.api
         ~namespace:"default" deployment_name);
    ignore
      (K.Dynamic.delete_collection ~namespace:"default" ~label_selector:selector
         client ~api:K8s.Core_v1.ConfigMap.api);
    K.Client.close client
  in
  Fun.protect ~finally:cleanup (fun () ->
      ignore
        (K.Dynamic.delete client ~api:K8s.Apps_v1.Deployment.api
           ~namespace:"default" deployment_name);
      ignore
        (K.Dynamic.delete_collection ~namespace:"default"
           ~label_selector:selector client ~api:K8s.Core_v1.ConfigMap.api);
      K.Dynamic.create client ~api:K8s.Core_v1.ConfigMap.api
        ~namespace:"default"
        (config_map "ocaml-k8s-delete-one")
      |> require "create first collection object"
      |> ignore;
      K.Dynamic.create client ~api:K8s.Core_v1.ConfigMap.api
        ~namespace:"default"
        (config_map "ocaml-k8s-delete-two")
      |> require "create second collection object"
      |> ignore;
      K.Dynamic.delete_collection ~namespace:"default" ~label_selector:selector
        client ~api:K8s.Core_v1.ConfigMap.api
      |> require "delete selected collection";
      let remaining =
        K.Dynamic.list_all ~namespace:"default" ~label_selector:selector client
          ~api:K8s.Core_v1.ConfigMap.api
        |> require "list after collection deletion"
      in
      if remaining.items <> [] then
        fail "collection deletion left selected ConfigMaps behind";
      K.Dynamic.create client ~api:K8s.Apps_v1.Deployment.api
        ~namespace:"default"
        (deployment deployment_name)
      |> require "create scalable Deployment"
      |> ignore;
      replace_scale_after_fresh_get client deployment_name 20 |> ignore;
      K.Dynamic.patch_scale client ~api:K8s.Apps_v1.Deployment.api
        ~namespace:"default" deployment_name
        (K.Client.Merge_patch
           (`Assoc [ ("spec", `Assoc [ ("replicas", `Int 0) ]) ]))
      |> require "patch Deployment Scale"
      |> ignore;
      let token =
        K.Dynamic.create_subresource client ~api:K8s.Core_v1.ServiceAccount.api
          ~namespace:"default" ~name:"default" ~subresource:"token"
          token_request
        |> require "create service-account token subresource"
      in
      if not (has_token token) then fail "TokenRequest response has no token";
      let system_pods =
        K.Dynamic.list_all ~namespace:"kube-system" client
          ~api:K8s.Core_v1.Pod.api
        |> require "list kube-system Pods"
      in
      let api_server =
        match
          List.find_opt
            (fun pod ->
              String.starts_with ~prefix:"kube-apiserver-"
                pod.K.Dynamic.metadata.name)
            system_pods.items
        with
        | Some pod -> pod
        | None -> fail "no kube-apiserver Pod was discovered"
      in
      let log_options =
        {
          K.Client.default_log_options with
          container = Some "kube-apiserver";
          tail_lines = Some 1L;
          limit_bytes = Some 8192L;
        }
      in
      let buffered =
        K.Dynamic.logs ~options:log_options ~max_body_bytes:16384 client
          ~api:K8s.Core_v1.Pod.api ~namespace:"kube-system"
          api_server.K.Dynamic.metadata.name
        |> require "read buffered Pod logs"
      in
      if buffered = "" then fail "buffered Pod log response was empty";
      let streamed = Buffer.create 1024 in
      K.Dynamic.stream_logs ~options:log_options client ~api:K8s.Core_v1.Pod.api
        ~namespace:"kube-system" api_server.K.Dynamic.metadata.name
        ~on_chunk:(Buffer.add_string streamed)
      |> require "stream Pod logs";
      if Buffer.length streamed = 0 then fail "streamed Pod logs were empty";
      Printf.printf
        "client features passed: collection delete, generic subresource, \
         Scale, buffered and streaming logs\n\
         %!")
