module K = Kube
module C = Kube_crd

module Spec = struct
  type t = { message : string }

  let schema =
    C.Schema.object_ ~required:[ "message" ]
      [ ("message", C.Schema.string ~min_length:1 ()) ]

  let of_json = function
    | `Assoc fields -> (
        match List.assoc_opt "message" fields with
        | Some (`String message) -> Ok { message }
        | _ -> Error "message is required")
    | _ -> Error "spec must be an object"

  let to_json value = `Assoc [ ("message", `String value.message) ]
end

module Status = struct
  type t = { observed_generation : int; reconciled_message : string }

  let schema =
    C.Schema.object_
      [
        ("observedGeneration", C.Schema.integer ~format:`Int64 ());
        ("reconciledMessage", C.Schema.string ());
      ]

  let of_json = function
    | `Assoc fields -> (
        match
          ( List.assoc_opt "observedGeneration" fields,
            List.assoc_opt "reconciledMessage" fields )
        with
        | Some (`Int observed_generation), Some (`String reconciled_message) ->
            Ok { observed_generation; reconciled_message }
        | _ -> Error "invalid status")
    | _ -> Error "status must be an object"

  let to_json value =
    `Assoc
      [
        ("observedGeneration", `Int value.observed_generation);
        ("reconciledMessage", `String value.reconciled_message);
      ]
end

module Greeting = C.Resource.Make (struct
  module Spec = Spec
  module Status = Status

  let group = "demo.ocaml-k8s.dev"
  let version = "v1alpha1"
  let kind = "Greeting"
  let plural = "greetings"
  let singular = "greeting"
  let scope = K.Core.Namespaced
  let short_names = [ "greet" ]
  let categories = [ "all" ]
end)

let metadata name =
  {
    K.Core.name;
    namespace = Some "default";
    uid = None;
    resource_version = None;
    generation = None;
    deletion_timestamp = None;
    finalizers = [];
    owner_references = [];
    labels = [];
    annotations = [];
  }

let test_resource_round_trip () =
  let value =
    Greeting.make ~metadata:(metadata "hello") ~spec:{ Spec.message = "hi" }
      ~status:
        { Status.observed_generation = 1; reconciled_message = "reconciled" }
      ()
  in
  match Greeting.of_json (Greeting.to_json value) with
  | Error message -> Alcotest.fail message
  | Ok decoded ->
      Alcotest.(check string) "name" "hello" decoded.metadata.name;
      Alcotest.(check string) "spec" "hi" decoded.spec.message;
      Alcotest.(check (option string))
        "status" (Some "reconciled")
        (Option.map
           (fun status -> status.Status.reconciled_message)
           decoded.status)

let test_schema_validation () =
  let invalid =
    C.Schema.object_ ~required:[ "missing"; "missing" ]
      [ ("present", C.Schema.string ()); ("present", C.Schema.boolean ()) ]
  in
  match C.Schema.validate invalid with
  | Ok () -> Alcotest.fail "invalid schema passed validation"
  | Error errors ->
      Alcotest.(check bool)
        "duplicate property reported" true
        (List.exists
           (String.starts_with ~prefix:"$: duplicate property present")
           errors);
      Alcotest.(check bool)
        "missing required property reported" true
        (List.exists
           (String.starts_with ~prefix:"$: required field has no schema")
           errors)

let test_manifest () =
  let json = C.Custom_resource_definition.to_json Greeting.crd in
  let member name = function
    | `Assoc fields -> List.assoc name fields
    | _ -> Alcotest.fail (name ^ " parent is not an object")
  in
  let metadata = member "metadata" json in
  Alcotest.(check string)
    "CRD name" "greetings.demo.ocaml-k8s.dev"
    (match member "name" metadata with
    | `String value -> value
    | _ -> Alcotest.fail "metadata.name is not a string");
  let yaml = C.Custom_resource_definition.to_yaml Greeting.crd in
  List.iter
    (fun expected ->
      Alcotest.(check bool)
        ("YAML contains " ^ expected)
        true
        (let length = String.length expected in
         let rec find index =
           index + length <= String.length yaml
           && (String.sub yaml index length = expected || find (index + 1))
         in
         find 0))
    [
      "apiVersion: apiextensions.k8s.io/v1";
      "name: greetings.demo.ocaml-k8s.dev";
      "openAPIV3Schema:";
      "minLength: 1";
    ]

let test_invalid_crd () =
  let result =
    C.Custom_resource_definition.make ~group:"INVALID" ~kind:"greeting"
      ~plural:"Greetings" ~scope:K.Core.Namespaced
      ~versions:
        [
          C.Custom_resource_definition.version ~name:"alpha"
            ~schema:(C.Schema.string ()) ();
        ]
      ()
  in
  match result with
  | Ok _ -> Alcotest.fail "invalid CRD was accepted"
  | Error errors ->
      Alcotest.(check bool) "several errors" true (List.length errors >= 4)

let () =
  Alcotest.run "kube.crd"
    [
      ( "crd",
        [
          Alcotest.test_case "typed resource round trip" `Quick
            test_resource_round_trip;
          Alcotest.test_case "schema validation" `Quick test_schema_validation;
          Alcotest.test_case "manifest and YAML" `Quick test_manifest;
          Alcotest.test_case "invalid definition" `Quick test_invalid_crd;
        ] );
    ]
