module K = Kube

let fail_error context error =
  Format.eprintf "%s: %a@." context K.Client.pp_error error;
  exit 1

let check_mapping (expected : K.Core.api) (mapping : K.Discovery.Mapper.mapping)
    =
  let actual = mapping.api in
  if
    actual.group <> expected.group
    || actual.version <> expected.version
    || actual.kind <> expected.kind
    || actual.plural <> expected.plural
    || actual.scope <> expected.scope
  then (
    Format.eprintf
      "discovery mismatch for %s: got group=%s version=%s kind=%s plural=%s@."
      expected.kind actual.group actual.version actual.kind actual.plural;
    exit 1)

let require_subresources mapping expected =
  let actual =
    List.map
      (fun subresource -> subresource.K.Discovery.Mapper.name)
      mapping.K.Discovery.Mapper.subresources
  in
  List.iter
    (fun required ->
      if not (List.mem required actual) then (
        Format.eprintf "discovery for %s has no %s subresource@."
          mapping.api.kind required;
        exit 1))
    expected

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
    "discovery-check [OPTIONS]";
  let config =
    match !kubeconfig with
    | Some path -> K.Config.load_kubeconfig ?context:!context path
    | None -> K.Config.load_default ?context:!context ()
  in
  let config =
    match config with
    | Ok config -> config
    | Error message ->
        Printf.eprintf "configuration error: %s\n%!" message;
        exit 2
  in
  let client = K.Client.create config in
  Fun.protect
    ~finally:(fun () -> K.Client.close client)
    (fun () ->
      let mapper = K.Discovery.Mapper.create client in
      (match K.Discovery.Mapper.preferred_version mapper ~group:"apps" with
      | Ok "v1" -> ()
      | Ok version ->
          Format.eprintf "unexpected preferred apps version: %s@." version;
          exit 1
      | Error error ->
          fail_error "apps preferred-version discovery failed" error);
      let deployment =
        match
          K.Discovery.Mapper.resolve_kind ~group:"apps" mapper
            ~kind:"Deployment"
        with
        | Ok mapping -> mapping
        | Error error ->
            fail_error "preferred Deployment discovery failed" error
      in
      check_mapping Kube_api_v1_36.Apps_v1.Deployment.api deployment;
      require_subresources deployment [ "scale"; "status" ];
      let greeting =
        match
          K.Discovery.Mapper.resolve_resource ~group:Greeting.api.group mapper
            ~resource:"greeting"
        with
        | Ok mapping -> mapping
        | Error error -> fail_error "Greeting alias discovery failed" error
      in
      check_mapping Greeting.api greeting;
      require_subresources greeting [ "status" ];
      Printf.printf
        "discovery passed: apps/v1 Deployment and %s/%s Greeting\n%!"
        Greeting.api.group Greeting.api.version)
