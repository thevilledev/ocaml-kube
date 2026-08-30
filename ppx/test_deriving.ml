module C = Kube_crd

module Spec = struct
  type t = {
    replicas : int;
    image : string;
    pull_policy : string option;
    ports : int list;
    labels : (string * string) list;
  }
  [@@deriving kube]
end

module State = struct
  type t = Pending | Ready [@kube.name "Available"] | Failed of string
  [@@deriving kube]
end

module Status = struct
  type t = {
    observed_generation : int64;
    state : State.t;
    detail : Yojson.Safe.t option;
  }
  [@@deriving kube]
end

module Alias = struct
  type t = string list [@@deriving kube]
end

module Tuple_alias = struct
  type t = int * string [@@deriving kube]
end

module Phase = struct
  type t = Pending | Running | Complete [@@deriving kube]
end

module Attributes = struct
  type t = {
    image_name : string;
        [@kube.key "image"]
        [@kube.schema C.Schema.string ~min_length:1 ()]
        [@kube.description "Container image reference."]
  }
  [@@deriving kube]
end

module Operation = struct
  type t =
    | Idle
    | Retry of int * string
    | Progress of { completed : int; total : int }
  [@@deriving kube]
end

module Tree = struct
  type t = {
    value : string;
    children : t list;
        [@kube.schema C.Schema.array (C.Schema.preserve_unknown ())]
  }
  [@@deriving kube]
end

module Mutual = struct
  type left = {
    name : string;
    right : right option; [@kube.schema C.Schema.preserve_unknown ()]
  }

  and right = {
    count : int;
    left : left option; [@kube.schema C.Schema.preserve_unknown ()]
  }
  [@@deriving kube]
end

module User_shape = struct
  type spec = { replicas : int; image : string } [@@deriving kube]
  type status = Pending | Ready | Failed of string [@@deriving kube]
end

module Codec_only = struct
  type mode = Active | Passive [@@deriving kube_json]

  type t = {
    display_name : string; [@kube.key "displayName"]
    mode : mode;
    ports : int list;
  }
  [@@deriving kube_json]
end

let test_record_codec () =
  let value =
    {
      Spec.replicas = 3;
      image = "example/operator:v1";
      pull_policy = Some "IfNotPresent";
      ports = [ 8080; 9090 ];
      labels = [ ("app", "demo") ];
    }
  in
  let json = Spec.to_json value in
  (match json with
  | `Assoc fields ->
      Alcotest.(check bool)
        "snake case becomes lower camel" true
        (List.mem_assoc "pullPolicy" fields);
      Alcotest.(check bool)
        "lower camel value" true
        (List.assoc_opt "pullPolicy" fields = Some (`String "IfNotPresent"));
      Alcotest.(check bool)
        "string map is object" true
        (match List.assoc_opt "labels" fields with
        | Some (`Assoc _) -> true
        | _ -> false)
  | _ -> Alcotest.fail "record did not encode as object");
  match Spec.of_json json with
  | Error message -> Alcotest.fail message
  | Ok decoded ->
      Alcotest.(check int) "replicas" 3 decoded.replicas;
      Alcotest.(check (list int)) "ports" [ 8080; 9090 ] decoded.ports;
      Alcotest.(check (option string))
        "optional" (Some "IfNotPresent") decoded.pull_policy;
      let absent = Spec.to_json { value with pull_policy = None } in
      Alcotest.(check bool)
        "None field omitted" true
        (match absent with
        | `Assoc fields -> not (List.mem_assoc "pullPolicy" fields)
        | _ -> false)

let test_variant_codec () =
  Alcotest.(check string)
    "renamed nullary tag" "Available"
    (match State.to_json State.Ready with
    | `Assoc fields -> (
        match List.assoc_opt "type" fields with
        | Some (`String value) -> value
        | _ -> Alcotest.fail "missing variant type")
    | _ -> Alcotest.fail "payload variant is not tagged");
  let failed = State.to_json (State.Failed "boom") in
  Alcotest.(check bool)
    "unary payload has a structural object envelope" true
    (failed
    = `Assoc
        [
          ("type", `String "Failed");
          ("value", `Assoc [ ("value", `String "boom") ]);
        ]);
  match State.of_json failed with
  | Ok (State.Failed "boom") -> ()
  | Ok _ -> Alcotest.fail "variant payload changed"
  | Error message -> Alcotest.fail message

let test_schema () =
  let spec = C.Schema.to_json Spec.schema |> Yojson.Safe.to_string in
  Alcotest.(check bool)
    "required fields emitted" true
    (String.length spec > 0 && String.contains spec 'r');
  let state = C.Schema.to_json State.schema in
  Alcotest.(check bool)
    "variant schema validates" true
    (C.Schema.validate State.schema = Ok ());
  Alcotest.(check bool)
    "tagged variant schema is object" true
    (match state with
    | `Assoc fields -> List.assoc_opt "type" fields = Some (`String "object")
    | _ -> false)

let test_status_round_trip () =
  let value =
    {
      Status.observed_generation = 9L;
      state = State.Failed "unavailable";
      detail = Some (`Assoc [ ("retry", `Bool true) ]);
    }
  in
  match Status.of_json (Status.to_json value) with
  | Error message -> Alcotest.fail message
  | Ok decoded ->
      Alcotest.(check int64) "int64" 9L decoded.observed_generation;
      Alcotest.(check bool) "raw JSON" true (decoded.detail = value.detail)

let test_alias () =
  match Alias.of_json (Alias.to_json [ "a"; "b" ]) with
  | Ok values -> Alcotest.(check (list string)) "alias" [ "a"; "b" ] values
  | Error message -> Alcotest.fail message

let test_tuple_alias () =
  let json = Tuple_alias.to_json (7, "seven") in
  Alcotest.(check bool)
    "tuple uses named structural fields" true
    (json = `Assoc [ ("item0", `Int 7); ("item1", `String "seven") ]);
  match Tuple_alias.of_json json with
  | Ok value ->
      Alcotest.(check bool) "tuple round trip" true (value = (7, "seven"))
  | Error message -> Alcotest.fail message

let test_attributes () =
  let json = Attributes.to_json { image_name = "repo/image:v1" } in
  Alcotest.(check bool)
    "explicit key" true
    (json = `Assoc [ ("image", `String "repo/image:v1") ]);
  let schema = C.Schema.to_json Attributes.schema in
  match schema with
  | `Assoc fields -> (
      match List.assoc_opt "properties" fields with
      | Some (`Assoc properties) -> (
          match List.assoc_opt "image" properties with
          | Some (`Assoc image) ->
              Alcotest.(check (option int))
                "schema override" (Some 1)
                (match List.assoc_opt "minLength" image with
                | Some (`Int value) -> Some value
                | _ -> None);
              Alcotest.(check (option string))
                "description" (Some "Container image reference.")
                (match List.assoc_opt "description" image with
                | Some (`String value) -> Some value
                | _ -> None)
          | _ -> Alcotest.fail "image schema missing")
      | _ -> Alcotest.fail "properties schema missing")
  | _ -> Alcotest.fail "attribute schema is not an object"

let test_variant_shapes () =
  Alcotest.(check bool)
    "nullary variant is enum string" true
    (Phase.to_json Phase.Running = `String "Running");
  (match Phase.of_json (`String "Complete") with
  | Ok Phase.Complete -> ()
  | Ok _ -> Alcotest.fail "wrong nullary constructor"
  | Error message -> Alcotest.fail message);
  List.iter
    (fun value ->
      match Operation.of_json (Operation.to_json value) with
      | Ok decoded ->
          Alcotest.(check bool)
            "payload variant round trip" true (decoded = value)
      | Error message -> Alcotest.fail message)
    [
      Operation.Idle; Retry (3, "later"); Progress { completed = 2; total = 5 };
    ]

let test_recursive_codec () =
  let value =
    {
      Tree.value = "root";
      children = [ { Tree.value = "leaf"; children = [] } ];
    }
  in
  match Tree.of_json (Tree.to_json value) with
  | Ok decoded -> Alcotest.(check bool) "recursive tree" true (decoded = value)
  | Error message -> Alcotest.fail message

let test_mutual_codec () =
  let value =
    {
      Mutual.name = "left";
      right =
        Some
          {
            Mutual.count = 2;
            left = Some { Mutual.name = "nested"; right = None };
          };
    }
  in
  match Mutual.left_of_json (Mutual.left_to_json value) with
  | Ok decoded -> Alcotest.(check bool) "mutual recursion" true (decoded = value)
  | Error message -> Alcotest.fail message

let test_interface_deriving () =
  let value = { Interface_fixture.display_name = "worker"; replicas = None } in
  match Interface_fixture.of_json (Interface_fixture.to_json value) with
  | Ok decoded ->
      Alcotest.(check string) "exported codec" "worker" decoded.display_name;
      Alcotest.(check bool)
        "exported schema" true
        (C.Schema.validate Interface_fixture.schema = Ok ())
  | Error message -> Alcotest.fail message

let test_named_types () =
  let spec = { User_shape.replicas = 2; image = "repo/image:v2" } in
  (match User_shape.spec_of_json (User_shape.spec_to_json spec) with
  | Ok decoded -> Alcotest.(check bool) "named record" true (decoded = spec)
  | Error message -> Alcotest.fail message);
  match
    User_shape.status_of_json
      (User_shape.status_to_json (User_shape.Failed "broken"))
  with
  | Ok (User_shape.Failed "broken") -> ()
  | Ok _ -> Alcotest.fail "named variant changed"
  | Error message -> Alcotest.fail message

let test_codec_only () =
  let value =
    {
      Codec_only.display_name = "worker";
      mode = Codec_only.Active;
      ports = [ 8080; 9090 ];
    }
  in
  let json = Codec_only.to_json value in
  Alcotest.(check bool)
    "wire name" true
    (match json with
    | `Assoc fields -> List.mem_assoc "displayName" fields
    | _ -> false);
  match Codec_only.of_json json with
  | Ok decoded ->
      Alcotest.(check bool) "codec-only round trip" true (decoded = value)
  | Error message -> Alcotest.fail message

let () =
  Alcotest.run "kube.ppx"
    [
      ( "deriving",
        [
          Alcotest.test_case "record codec" `Quick test_record_codec;
          Alcotest.test_case "variant codec" `Quick test_variant_codec;
          Alcotest.test_case "schema" `Quick test_schema;
          Alcotest.test_case "status round trip" `Quick test_status_round_trip;
          Alcotest.test_case "alias" `Quick test_alias;
          Alcotest.test_case "tuple alias" `Quick test_tuple_alias;
          Alcotest.test_case "attributes" `Quick test_attributes;
          Alcotest.test_case "variant shapes" `Quick test_variant_shapes;
          Alcotest.test_case "recursive codec" `Quick test_recursive_codec;
          Alcotest.test_case "mutual codec" `Quick test_mutual_codec;
          Alcotest.test_case "interface deriving" `Quick test_interface_deriving;
          Alcotest.test_case "named types" `Quick test_named_types;
          Alcotest.test_case "codec-only derivation" `Quick test_codec_only;
        ] );
    ]
