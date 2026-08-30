module K = Kube

let resource_key (api : K.Core.api) =
  String.concat "/" [ api.group; api.version; api.plural ]

let check_registry version expected resources =
  Alcotest.(check int)
    (version ^ " resource count")
    expected (List.length resources);
  let keys = List.map resource_key resources in
  let unique = List.sort_uniq String.compare keys in
  Alcotest.(check int) (version ^ " unique GVRs") expected (List.length unique);
  Alcotest.(check bool)
    (version ^ " has Pods") true
    (List.exists
       (fun (api : K.Core.api) ->
         api.group = "" && api.version = "v1" && api.plural = "pods"
         && api.scope = K.Core.Namespaced)
       resources);
  Alcotest.(check bool)
    (version ^ " has Deployments")
    true
    (List.exists
       (fun (api : K.Core.api) ->
         api.group = "apps" && api.version = "v1" && api.plural = "deployments"
         && api.scope = K.Core.Namespaced)
       resources)

let test_all_supported_minors () =
  check_registry "1.34" 58 Kube_api_v1_34.all_resources;
  check_registry "1.35" 58 Kube_api_v1_35.all_resources;
  check_registry "1.36" 60 Kube_api_v1_36.all_resources;
  check_registry "1.37" 64 Kube_api_v1_37.all_resources

let () =
  Alcotest.run "generated API matrix"
    [
      ( "registry",
        [
          Alcotest.test_case "supported minors" `Quick test_all_supported_minors;
        ] );
    ]
