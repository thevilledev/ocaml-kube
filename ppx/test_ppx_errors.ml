open Ppxlib

let parse_type source =
  let lexbuf = Lexing.from_string source in
  Location.init lexbuf "fixture.ml";
  match Parse.implementation lexbuf with
  | [ { pstr_desc = Pstr_type (rec_flag, declarations); _ } ] ->
      (rec_flag, declarations)
  | _ -> Alcotest.fail "fixture is not one type declaration"

let generated source =
  Kube_ppx.generate_structure ~include_schema:true ~loc:Location.none
    ~path:"fixture" (parse_type source)

let contains value substring =
  let length = String.length substring in
  let rec loop index =
    index + length <= String.length value
    && (String.sub value index length = substring || loop (index + 1))
  in
  loop 0

let check_error expected source =
  try
    ignore (generated source);
    Alcotest.fail ("expected deriving error containing: " ^ expected)
  with Location.Error error ->
    let message = Location.Error.message error in
    Alcotest.(check bool)
      ("diagnostic contains " ^ expected)
      true
      (contains message expected)

let test_recursive_schema () =
  check_error "recursive or mutually recursive CRD schema reference"
    "type t = { children : t list }"

let test_duplicate_field () =
  check_error "duplicate JSON field name"
    "type t = { first : string [@kube.key \"same\"]; second : int [@kube.key \
     \"same\"] }"

let test_duplicate_constructor () =
  check_error "duplicate variant tag"
    "type t = First [@kube.name \"same\"] | Second [@kube.name \"same\"]"

let test_type_parameter () =
  check_error "does not yet support parameterized type declarations"
    "type 'a t = { value : 'a }"

let test_unsupported_builtin () =
  check_error "does not support char" "type t = { value : char }"

let () =
  Alcotest.run "kube.ppx diagnostics"
    [
      ( "errors",
        [
          Alcotest.test_case "recursive schema" `Quick test_recursive_schema;
          Alcotest.test_case "duplicate field" `Quick test_duplicate_field;
          Alcotest.test_case "duplicate constructor" `Quick
            test_duplicate_constructor;
          Alcotest.test_case "type parameter" `Quick test_type_parameter;
          Alcotest.test_case "unsupported builtin" `Quick
            test_unsupported_builtin;
        ] );
    ]
