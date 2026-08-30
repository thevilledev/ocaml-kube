module Api = Kube_api_v1_36

let fail_error context = function
  | Ok value -> value
  | Error message -> Alcotest.failf "%s: %s" context message

let test_config_map_round_trip () =
  let metadata =
    Api.Meta_v1.ObjectMeta.make ~generation:7L
      ~labels:[ ("app", "demo") ]
      ~name:"settings" ~namespace:"operators" ~resource_version:"42" ()
  in
  let value =
    Api.Core_v1.ConfigMap.make ~api_version:"v1"
      ~binary_data:[ ("certificate", "Ynl0ZXM=") ]
      ~data:[ ("config", "enabled=true") ]
      ~immutable:true ~kind:"ConfigMap" ~metadata ()
  in
  let encoded = Api.Core_v1.ConfigMap.to_json value in
  let decoded =
    Api.Core_v1.ConfigMap.of_json encoded |> fail_error "decode ConfigMap"
  in
  Alcotest.(check string)
    "stable round trip"
    (Yojson.Safe.to_string encoded)
    (Yojson.Safe.to_string (Api.Core_v1.ConfigMap.to_json decoded));
  let core_metadata = Api.Core_v1.ConfigMap.metadata decoded in
  Alcotest.(check string) "metadata name" "settings" core_metadata.name;
  Alcotest.(check (option string))
    "metadata namespace" (Some "operators") core_metadata.namespace;
  Alcotest.(check (option int))
    "int64 generation" (Some 7) core_metadata.generation;
  Alcotest.(check (list (pair string string)))
    "metadata labels"
    [ ("app", "demo") ]
    core_metadata.labels

let test_resource_descriptors () =
  let config_map = Api.Core_v1.ConfigMap.api in
  Alcotest.(check string) "core group" "" config_map.group;
  Alcotest.(check string) "core version" "v1" config_map.version;
  Alcotest.(check string) "ConfigMap plural" "configmaps" config_map.plural;
  Alcotest.(check bool)
    "ConfigMap namespaced" true
    (config_map.scope = Kube.Core.Namespaced);
  Alcotest.(check (result string string))
    "namespaced path" (Ok "/api/v1/namespaces/operators/configmaps")
    (Kube.Core.collection_path config_map ~namespace:(Some "operators"));
  let namespace = Api.Core_v1.Namespace.api in
  Alcotest.(check bool)
    "Namespace cluster scoped" true
    (namespace.scope = Kube.Core.Cluster);
  Alcotest.(check (result string string))
    "cluster path" (Ok "/api/v1/namespaces")
    (Kube.Core.collection_path namespace ~namespace:None);
  Alcotest.(check string) "apps group" "apps" Api.Apps_v1.Deployment.api.group;
  Alcotest.(check string)
    "coordination group" "coordination.k8s.io"
    Api.Coordination_v1.Lease.api.group

let test_complete_stable_resource_registry () =
  Alcotest.(check int)
    "stable LIST/WATCH resources" 60
    (List.length Api.all_resources);
  let identities =
    List.map
      (fun api ->
        String.concat "/" [ api.Kube.Core.group; api.version; api.plural ])
      Api.all_resources
  in
  Alcotest.(check int)
    "unique GVRs" 60
    (List.length (List.sort_uniq String.compare identities));
  let check_descriptor label expected api =
    let actual =
      Printf.sprintf "%s/%s/%s/%s" api.Kube.Core.group api.version api.plural
        (match api.scope with
        | Kube.Core.Namespaced -> "Namespaced"
        | Kube.Core.Cluster -> "Cluster")
    in
    Alcotest.(check string) label expected actual
  in
  check_descriptor "CRD"
    "apiextensions.k8s.io/v1/customresourcedefinitions/Cluster"
    Api.Apiextensions_v1.CustomResourceDefinition.api;
  check_descriptor "validating policy"
    "admissionregistration.k8s.io/v1/validatingadmissionpolicies/Cluster"
    Api.Admissionregistration_v1.ValidatingAdmissionPolicy.api;
  check_descriptor "resource claim"
    "resource.k8s.io/v1/resourceclaims/Namespaced"
    Api.Resource_v1.ResourceClaim.api;
  check_descriptor "storage capacity"
    "storage.k8s.io/v1/csistoragecapacities/Namespaced"
    Api.Storage_v1.CSIStorageCapacity.api

let test_int_or_string () =
  let numeric =
    Api.Int_or_string.of_json (`Int 8080) |> fail_error "decode numeric port"
  in
  let named =
    Api.Int_or_string.of_json (`String "http") |> fail_error "decode named port"
  in
  Alcotest.(check string)
    "numeric encoding" "8080"
    (Yojson.Safe.to_string (Api.Int_or_string.to_json numeric));
  Alcotest.(check string)
    "named encoding" {|"http"|}
    (Yojson.Safe.to_string (Api.Int_or_string.to_json named));
  match Api.Int_or_string.of_json (`Bool true) with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "boolean unexpectedly decoded as IntOrString"

let test_required_fields () =
  match Api.io_k8s_api_core_v1_container_port_of_json (`Assoc []) with
  | Error message ->
      Alcotest.(check bool)
        "path-rich required-field error" true
        (String.starts_with ~prefix:"containerPort:" message)
  | Ok _ -> Alcotest.fail "ContainerPort decoded without containerPort"

let test_raw_json_preservation () =
  let raw =
    `Assoc
      [
        ("f:spec", `Assoc [ ("f:containers", `Assoc []) ]);
        ("future-field", `List [ `Int 1; `String "two" ]);
      ]
  in
  let decoded =
    Api.io_k8s_apimachinery_pkg_apis_meta_v1_fields_v1_of_json raw
    |> fail_error "decode FieldsV1"
  in
  Alcotest.(check string)
    "unstructured field set"
    (Yojson.Safe.to_string raw)
    (Yojson.Safe.to_string
       (Api.io_k8s_apimachinery_pkg_apis_meta_v1_fields_v1_to_json decoded))

let () =
  Alcotest.run "generated Kubernetes API"
    [
      ( "codecs",
        [
          Alcotest.test_case "ConfigMap round trip" `Quick
            test_config_map_round_trip;
          Alcotest.test_case "IntOrString" `Quick test_int_or_string;
          Alcotest.test_case "required fields" `Quick test_required_fields;
          Alcotest.test_case "raw JSON preservation" `Quick
            test_raw_json_preservation;
        ] );
      ( "resources",
        [
          Alcotest.test_case "descriptors" `Quick test_resource_descriptors;
          Alcotest.test_case "complete stable registry" `Quick
            test_complete_stable_resource_registry;
        ] );
    ]
