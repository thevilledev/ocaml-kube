let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let fixture name =
  let local = Filename.concat "fixtures" name in
  if Sys.file_exists local then local
  else Filename.concat "codegen/fixtures" name

let load path =
  match Openapi_codegen.load_json path with
  | Ok value -> value
  | Error message -> Alcotest.fail message

let generate schema manifest =
  match Openapi_codegen.generate ~schema ~manifest with
  | Ok output -> output
  | Error message -> Alcotest.fail message

let test_golden_output () =
  let output =
    generate (load (fixture "swagger.json")) (load (fixture "manifest.json"))
  in
  Alcotest.(check int) "dependency closure" 4 output.definition_count;
  Alcotest.(check string)
    "implementation"
    (read (fixture "expected.ml"))
    output.implementation;
  Alcotest.(check string)
    "interface"
    (read (fixture "expected.mli"))
    output.interface;
  Alcotest.(check string)
    "pruned schema"
    (read (fixture "pruned.json"))
    output.pruned_schema

let test_pruned_schema_reproduces_output () =
  let manifest = load (fixture "manifest.json") in
  let full = generate (load (fixture "swagger.json")) manifest in
  let pruned = generate (Yojson.Safe.from_string full.pruned_schema) manifest in
  Alcotest.(check string)
    "implementation" full.implementation pruned.implementation;
  Alcotest.(check string) "interface" full.interface pruned.interface;
  Alcotest.(check string)
    "schema fixed point" full.pruned_schema pruned.pruned_schema

let test_missing_reference_fails () =
  let schema =
    `Assoc
      [
        ("swagger", `String "2.0");
        ( "definitions",
          `Assoc
            [
              ( "example.v1.Widget",
                `Assoc
                  [
                    ("type", `String "object");
                    ( "properties",
                      `Assoc
                        [
                          ( "missing",
                            `Assoc
                              [
                                ( "$ref",
                                  `String "#/definitions/example.v1.Missing" );
                              ] );
                        ] );
                    ( "x-kubernetes-group-version-kind",
                      `List
                        [
                          `Assoc
                            [
                              ("group", `String "example.dev");
                              ("version", `String "v1");
                              ("kind", `String "Widget");
                            ];
                        ] );
                  ] );
            ] );
      ]
  in
  match
    Openapi_codegen.generate ~schema ~manifest:(load (fixture "manifest.json"))
  with
  | Error message ->
      Alcotest.(check bool)
        "names missing definition" true
        (String.ends_with ~suffix:"example.v1.Missing" message)
  | Ok _ -> Alcotest.fail "generation unexpectedly accepted a missing reference"

let test_stable_resource_derivation () =
  let schema = load (fixture "swagger.json") in
  let manifest = load (fixture "manifest.json") in
  let derived =
    match Openapi_codegen.derive_stable_manifest ~schema ~manifest with
    | Ok value -> value
    | Error message -> Alcotest.fail message
  in
  let open Yojson.Safe.Util in
  let resource value =
    Printf.sprintf "%s|%s|%s|%s"
      (value |> member "definition" |> to_string)
      (value |> member "module" |> to_string)
      (value |> member "plural" |> to_string)
      (value |> member "scope" |> to_string)
  in
  let resources =
    derived |> member "resources" |> to_list |> List.map resource
  in
  Alcotest.(check (list string))
    "stable resources and scopes"
    [
      "example.v1.Widget|Example_v1|widgets|Namespaced";
      "example.v2.ClusterWidget|Example_v2|clusterwidgets|Cluster";
    ]
    resources;
  Alcotest.(check string)
    "manifest metadata retained" "fixture-sha256"
    (derived |> member "sha256" |> to_string);
  let bootstrapped =
    match
      Openapi_codegen.derive_stable_manifest_with_metadata ~schema
        ~kubernetes_version:"v9.9.9" ~source:"https://example.test/swagger.json"
        ~sha256:(String.make 64 'a')
    with
    | Ok value -> value
    | Error message -> Alcotest.fail message
  in
  Alcotest.(check string)
    "bootstrapped version" "v9.9.9"
    (bootstrapped |> member "kubernetesVersion" |> to_string);
  Alcotest.(check (list string))
    "bootstrapped resource derivation" resources
    (bootstrapped |> member "resources" |> to_list |> List.map resource);
  match Openapi_codegen.derive_stable_manifest ~schema ~manifest:derived with
  | Error message -> Alcotest.fail message
  | Ok repeated ->
      Alcotest.(check string)
        "deterministic fixed point"
        (Yojson.Safe.pretty_to_string derived)
        (Yojson.Safe.pretty_to_string repeated)

let () =
  Alcotest.run "OpenAPI code generator"
    [
      ( "generation",
        [
          Alcotest.test_case "golden output" `Quick test_golden_output;
          Alcotest.test_case "pruned schema fixed point" `Quick
            test_pruned_schema_reproduces_output;
          Alcotest.test_case "missing reference" `Quick
            test_missing_reference_fails;
          Alcotest.test_case "stable resource derivation" `Quick
            test_stable_resource_derivation;
        ] );
    ]
