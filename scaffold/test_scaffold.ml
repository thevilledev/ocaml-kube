let fixture = "fixtures/widget-crd.yaml"

type corpus_case = {
  name : string;
  fixture : string;
  project_name : string;
  group : string;
  version : string;
  kind : string;
  diagnostic_paths : string list;
}

let corpus =
  [
    {
      name = "Gateway API GatewayClass v1.5.1";
      fixture = "fixtures/real-world/gateway-api-gatewayclass-v1.5.1.yaml";
      project_name = "gateway-class-operator";
      group = "gateway.networking.k8s.io";
      version = "v1";
      kind = "GatewayClass";
      diagnostic_paths = [];
    };
    {
      name = "KEDA ScaledObject v2.20.1";
      fixture = "fixtures/real-world/keda-scaledobject-v2.20.1.yaml";
      project_name = "scaled-object-operator";
      group = "keda.sh";
      version = "v1alpha1";
      kind = "ScaledObject";
      diagnostic_paths = [];
    };
    {
      name = "Prometheus Operator ServiceMonitor v0.93.0";
      fixture = "fixtures/real-world/prometheus-servicemonitor-v0.93.0.yaml";
      project_name = "service-monitor-operator";
      group = "monitoring.coreos.com";
      version = "v1";
      kind = "ServiceMonitor";
      diagnostic_paths =
        [
          ".spec.endpoints[].metricRelabelings[].action";
          ".spec.endpoints[].relabelings[].action";
        ];
    };
  ]

let definition () =
  match Kube_scaffold.load_crd fixture with
  | Ok value -> value
  | Error message -> Alcotest.fail message

let find_file path files =
  match List.assoc_opt path files with
  | Some value -> value
  | None -> Alcotest.fail ("missing generated file " ^ path)

let contains value substring =
  let rec loop index =
    if String.length substring = 0 then true
    else if index + String.length substring > String.length value then false
    else if String.sub value index (String.length substring) = substring then
      true
    else loop (index + 1)
  in
  loop 0

let test_corpus_case case () =
  let definition =
    match Kube_scaffold.load_crd case.fixture with
    | Ok value -> value
    | Error message -> Alcotest.failf "%s: %s" case.name message
  in
  Alcotest.(check string)
    "project name" case.project_name
    (Kube_scaffold.default_project_name definition);
  Alcotest.(check (list string))
    "dynamic fallbacks" case.diagnostic_paths
    (Kube_scaffold.diagnostics definition
    |> List.map (fun (diagnostic : Kube_scaffold.diagnostic) -> diagnostic.path)
    );
  let files =
    match Kube_scaffold.render definition with
    | Ok value -> value
    | Error message -> Alcotest.failf "%s: %s" case.name message
  in
  Alcotest.(check int) "complete generated project" 13 (List.length files);
  let resource = find_file "lib/custom_resource.ml" files in
  List.iter
    (fun expected ->
      Alcotest.(check bool) expected true (contains resource expected))
    [
      Printf.sprintf "let group = %S" case.group;
      Printf.sprintf "let version = %S" case.version;
      Printf.sprintf "let kind = %S" case.kind;
      "[@@deriving kube_json]";
    ]

let test_render () =
  let definition = definition () in
  Alcotest.(check string)
    "default project" "widget-operator"
    (Kube_scaffold.default_project_name definition);
  let diagnostics = Kube_scaffold.diagnostics definition in
  Alcotest.(check (list (pair string string)))
    "explicit raw fallbacks"
    [
      ( ".spec.opaque",
        "x-kubernetes-preserve-unknown-fields requires lossless dynamic JSON" );
      ( ".spec.embeddedObject",
        "embedded Kubernetes resources require dynamic metadata preservation" );
    ]
    (List.map
       (fun (diagnostic : Kube_scaffold.diagnostic) ->
         (diagnostic.path, diagnostic.reason))
       diagnostics);
  let files =
    match Kube_scaffold.render definition with
    | Ok value -> value
    | Error message -> Alcotest.fail message
  in
  Alcotest.(check int) "complete project" 13 (List.length files);
  let resource = find_file "lib/custom_resource.ml" files in
  Alcotest.(check bool)
    "typed spec record" true
    (contains resource "image : string");
  Alcotest.(check bool)
    "codec-only derivation" true
    (contains resource "[@@deriving kube_json]");
  Alcotest.(check bool)
    "typed int32" true
    (contains resource "replicas : int32");
  Alcotest.(check bool)
    "nested object" true
    (contains resource "enabled : bool");
  Alcotest.(check bool)
    "wire key retained" true
    (contains resource "[@kube.key \"endpointURL\"]");
  Alcotest.(check bool)
    "raw fallback" true
    (contains resource "opaque : Raw_json.t option");
  Alcotest.(check bool)
    "embedded resource fallback" true
    (contains resource "embedded_object : Raw_json.t option");
  Alcotest.(check bool)
    "same-type structural validation remains typed" true
    (contains resource "choice : string option");
  Alcotest.(check bool)
    "Int-or-String mapping" true
    (contains resource "target_port : Int_or_string.t option");
  Alcotest.(check bool)
    "empty object mapping" true
    (contains resource "empty_config : Empty_object.t option");
  Alcotest.(check bool)
    "fallbacks documented in source" true
    (contains resource "Dynamic JSON fallbacks selected by the scaffolder");
  Alcotest.(check bool)
    "status int64" true
    (contains resource "observed_generation : int64");
  let operator = find_file "bin/main.ml" files in
  Alcotest.(check bool)
    "controller skeleton" true
    (contains operator "K.Controller.Make (Custom_resource)");
  Alcotest.(check bool)
    "finalizer skeleton" true
    (contains operator "Finalizer.run ~cancel:request.cancel");
  Alcotest.(check bool)
    "leader election" true
    (contains operator "K.Leader_election.run ~cancel ~on_phase");
  Alcotest.(check bool)
    "diagnostics" true
    (contains operator "K.Diagnostics.component diagnostics");
  Alcotest.(check bool)
    "cache readiness" true
    (contains operator "~metrics ~health ~reconcile");
  let rbac = find_file "deploy/rbac.yaml" files in
  Alcotest.(check bool)
    "Lease RBAC" true
    (contains rbac "resources: [\"leases\"]");
  let deployment = find_file "deploy/deployment.yaml" files in
  Alcotest.(check bool) "HA replicas" true (contains deployment "replicas: 2");
  Alcotest.(check bool)
    "leader election enabled" true
    (contains deployment "- --leader-elect");
  Alcotest.(check bool)
    "health probes" true
    (contains deployment "path: /healthz" && contains deployment "path: /readyz");
  Alcotest.(check bool)
    "restricted container" true
    (contains deployment "readOnlyRootFilesystem: true")

let test_init_render () =
  let files =
    match Kube_scaffold.render_init ~group:"example.dev" ~kind:"Widget" () with
    | Ok files -> files
    | Error message -> Alcotest.fail message
  in
  Alcotest.(check int) "complete type-first project" 15 (List.length files);
  Alcotest.(check bool)
    "default package name" true
    (List.mem_assoc "widget-operator.opam" files);
  let resource = find_file "lib/custom_resource.ml" files in
  Alcotest.(check bool)
    "type-first marker" true
    (contains resource "Generated by ocaml-k8s init");
  Alcotest.(check bool)
    "schema deriving" true
    (contains resource "[@@deriving kube]");
  Alcotest.(check bool)
    "typed spec" true
    (contains resource "replicas : int" && contains resource "image : string");
  Alcotest.(check bool)
    "typed status" true
    (contains resource "type t = Pending | Ready | Failed of string");
  let tools = find_file "tools/dune" files in
  Alcotest.(check bool)
    "CRD drift gate" true
    (contains tools "alias crd-check"
    && contains tools "diff ../deploy/crd.yaml");
  let generator = find_file "tools/generate_crd.ml" files in
  Alcotest.(check bool)
    "CRD comes from typed resource" true
    (contains generator "Custom_resource.crd");
  let crd = find_file "deploy/crd.yaml" files in
  Alcotest.(check bool)
    "default CRD identity" true
    (contains crd "name: widgets.example.dev" && contains crd "name: v1alpha1");
  Alcotest.(check bool)
    "invalid API identity rejected" true
    (Result.is_error
       (Kube_scaffold.render_init ~group:"INVALID" ~kind:"widget" ()))

let rec remove_tree path =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_DIR ->
      Sys.readdir path
      |> Array.iter (fun name -> remove_tree (Filename.concat path name));
      Unix.rmdir path
  | _ -> Unix.unlink path

let test_generate () =
  let parent = Filename.temp_dir "ocaml-k8s-scaffold-" "" in
  Fun.protect
    ~finally:(fun () -> remove_tree parent)
    (fun () ->
      let output = Filename.concat parent "operator" in
      let paths =
        match Kube_scaffold.generate ~output (definition ()) with
        | Ok value -> value
        | Error message -> Alcotest.fail message
      in
      Alcotest.(check int) "written files" 13 (List.length paths);
      Alcotest.(check bool)
        "resource exists" true
        (Sys.file_exists (Filename.concat output "lib/custom_resource.ml"));
      Alcotest.(check bool)
        "refuses overwrite" true
        (Result.is_error (Kube_scaffold.generate ~output (definition ()))))

let test_validation () =
  Alcotest.(check bool)
    "invalid project name" true
    (Result.is_error
       (Kube_scaffold.render ~project_name:"Invalid_Name" (definition ())));
  Alcotest.(check bool)
    "project name exceeds Kubernetes label limit" true
    (Result.is_error
       (Kube_scaffold.render ~project_name:(String.make 64 'a') (definition ())));
  let source =
    {|{"apiVersion":"apiextensions.k8s.io/v1","kind":"CustomResourceDefinition","metadata":{"name":"things.example.dev"},"spec":{"group":"example.dev","scope":"Namespaced","names":{"plural":"things","singular":"thing","kind":"Thing"},"versions":[{"name":"v1","served":true,"storage":true,"schema":{"openAPIV3Schema":{"type":"object","required":["spec"],"properties":{"spec":{"type":"object"},"status":{"type":"object"}}}}}]}}|}
  in
  let path = Filename.temp_file "ocaml-k8s-crd-" ".json" in
  let channel = open_out_bin path in
  output_string channel source;
  close_out channel;
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      match Kube_scaffold.load_crd path with
      | Error message ->
          Alcotest.(check bool)
            "requires status subresource" true
            (contains message "must enable the status subresource")
      | Ok _ -> Alcotest.fail "CRD without status subresource was accepted")

let () =
  Alcotest.run "ocaml-k8s scaffold"
    [
      ( "project",
        [
          Alcotest.test_case "render" `Quick test_render;
          Alcotest.test_case "type-first init" `Quick test_init_render;
          Alcotest.test_case "atomic generation" `Quick test_generate;
          Alcotest.test_case "validation" `Quick test_validation;
        ] );
      ( "real-world corpus",
        List.map
          (fun case ->
            Alcotest.test_case case.name `Quick (test_corpus_case case))
          corpus );
    ]
