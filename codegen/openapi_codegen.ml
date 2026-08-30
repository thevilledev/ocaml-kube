module String_map = Map.Make (String)
module String_set = Set.Make (String)

type output = {
  implementation : string;
  interface : string;
  pruned_schema : string;
  definition_count : int;
}

exception Generate_error of string

let fail format =
  Printf.ksprintf (fun message -> raise (Generate_error message)) format

let protect fn = try Ok (fn ()) with Generate_error message -> Error message

let assoc context = function
  | `Assoc fields -> fields
  | _ -> fail "%s must be an object" context

let list context = function
  | `List values -> values
  | _ -> fail "%s must be an array" context

let string context = function
  | `String value -> value
  | _ -> fail "%s must be a string" context

let member name fields = List.assoc_opt name fields

let required_member context name fields =
  match member name fields with
  | Some value -> value
  | None -> fail "%s is missing %s" context name

let required_string context name fields =
  required_member context name fields |> string (context ^ "." ^ name)

let optional_string name fields =
  match member name fields with
  | Some (`String value) -> Some value
  | _ -> None

let string_list context = function
  | `List values -> List.map (string context) values
  | _ -> fail "%s must be an array of strings" context

let load_json path =
  try Ok (Yojson.Safe.from_file path) with
  | Sys_error message -> Error (path ^ ": " ^ message)
  | Yojson.Json_error message -> Error (path ^ ": " ^ message)

let write_file path contents =
  try
    let channel = open_out_bin path in
    Fun.protect
      ~finally:(fun () -> close_out_noerr channel)
      (fun () -> output_string channel contents);
    Ok ()
  with Sys_error message -> Error (path ^ ": " ^ message)

let read_file path =
  try
    let channel = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr channel)
      (fun () -> Ok (really_input_string channel (in_channel_length channel)))
  with Sys_error message -> Error (path ^ ": " ^ message)

let check_file path expected =
  match read_file path with
  | Error _ as error -> error
  | Ok actual when actual = expected -> Ok ()
  | Ok _ -> Error (path ^ " is stale; regenerate the Kubernetes API package")

type scope = Namespaced | Cluster

type resource = {
  definition : string;
  module_name : string;
  plural : string;
  scope : scope;
}

type manifest = {
  kubernetes_version : string;
  source : string;
  sha256 : string;
  resources : resource list;
}

let valid_module_name value =
  let length = String.length value in
  let valid_tail = function
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' -> true
    | _ -> false
  in
  length > 0
  && (match value.[0] with
    | 'A' .. 'Z' -> true
    | _ -> false)
  && String.for_all valid_tail value

let parse_resource index json =
  let context = Printf.sprintf "manifest.resources[%d]" index in
  let fields = assoc context json in
  let definition = required_string context "definition" fields in
  let module_name = required_string context "module" fields in
  if not (valid_module_name module_name) then
    fail "%s.module is not a valid OCaml module name: %s" context module_name;
  let plural = required_string context "plural" fields in
  if plural = "" then fail "%s.plural must not be empty" context;
  let scope =
    match required_string context "scope" fields with
    | "Namespaced" -> Namespaced
    | "Cluster" -> Cluster
    | value ->
        fail "%s.scope must be Namespaced or Cluster, got %s" context value
  in
  { definition; module_name; plural; scope }

let parse_manifest json =
  let fields = assoc "manifest" json in
  let resources =
    required_member "manifest" "resources" fields
    |> list "manifest.resources" |> List.mapi parse_resource
  in
  if resources = [] then fail "manifest.resources must not be empty";
  let seen = Hashtbl.create (List.length resources) in
  List.iter
    (fun resource ->
      let key = resource.module_name ^ "." ^ resource.definition in
      if Hashtbl.mem seen key then fail "duplicate resource selection: %s" key;
      Hashtbl.add seen key ())
    resources;
  {
    kubernetes_version = required_string "manifest" "kubernetesVersion" fields;
    source = required_string "manifest" "source" fields;
    sha256 = required_string "manifest" "sha256" fields;
    resources;
  }

let definitions_of_schema json =
  let fields = assoc "OpenAPI document" json in
  (match optional_string "swagger" fields with
  | Some "2.0" -> ()
  | Some value -> fail "unsupported Swagger version %s (expected 2.0)" value
  | None -> fail "OpenAPI document is missing swagger");
  let definitions =
    required_member "OpenAPI document" "definitions" fields
    |> assoc "OpenAPI document.definitions"
  in
  List.fold_left
    (fun accumulator (name, schema) ->
      if String_map.mem name accumulator then
        fail "duplicate definition %s" name;
      String_map.add name schema accumulator)
    String_map.empty definitions

let ref_prefix = "#/definitions/"

let definition_of_ref value =
  if String.starts_with ~prefix:ref_prefix value then
    Some
      (String.sub value (String.length ref_prefix)
         (String.length value - String.length ref_prefix))
  else None

let required_definition_ref context json =
  let fields = assoc context json in
  match required_member context "$ref" fields with
  | `String reference -> (
      match definition_of_ref reference with
      | Some definition -> definition
      | None -> fail "%s has unsupported reference %s" context reference)
  | _ -> fail "%s.$ref must be a string" context

let stable_version version =
  let length = String.length version in
  length > 1
  && version.[0] = 'v'
  && String.for_all
       (function
         | '0' .. '9' -> true
         | _ -> false)
       (String.sub version 1 (length - 1))

let contains value needle =
  let value_length = String.length value in
  let needle_length = String.length needle in
  let rec search index =
    index + needle_length <= value_length
    && (String.sub value index needle_length = needle || search (index + 1))
  in
  needle_length = 0 || search 0

let path_last_segment context path =
  match List.rev (String.split_on_char '/' path) with
  | value :: _ when value <> "" -> value
  | _ -> fail "%s is not a collection path: %s" context path

let path_without_last_segment context path =
  let plural = path_last_segment context path in
  String.sub path 0 (String.length path - String.length plural - 1)

let operation path action path_fields =
  match member action path_fields with
  | Some value -> Some (assoc (path ^ "." ^ action) value)
  | None -> None

let operation_is path action path_fields expected =
  match operation path action path_fields with
  | Some fields -> optional_string "x-kubernetes-action" fields = Some expected
  | None -> false

let operation_gvk context fields =
  let fields =
    required_member context "x-kubernetes-group-version-kind" fields
    |> assoc (context ^ ".x-kubernetes-group-version-kind")
  in
  ( required_string (context ^ " GVK") "group" fields,
    required_string (context ^ " GVK") "version" fields,
    required_string (context ^ " GVK") "kind" fields )

let resource_module group version =
  let group_name =
    if group = "" then "Core"
    else
      match String.split_on_char '.' group with
      | name :: _ when name <> "" -> String.capitalize_ascii name
      | _ -> fail "invalid Kubernetes API group %S" group
  in
  let module_name = group_name ^ "_" ^ version in
  if not (valid_module_name module_name) then
    fail "derived OCaml module name is invalid: %s" module_name;
  module_name

let list_item_definition definitions context operation_fields =
  let responses =
    required_member context "responses" operation_fields
    |> assoc (context ^ ".responses")
  in
  let response =
    required_member (context ^ ".responses") "200" responses
    |> assoc (context ^ ".responses.200")
  in
  let list_definition =
    required_member (context ^ ".responses.200") "schema" response
    |> required_definition_ref (context ^ ".responses.200.schema")
  in
  let list_schema =
    match String_map.find_opt list_definition definitions with
    | Some schema -> assoc list_definition schema
    | None -> fail "list response definition not found: %s" list_definition
  in
  let properties =
    required_member list_definition "properties" list_schema
    |> assoc (list_definition ^ ".properties")
  in
  let items_property =
    required_member (list_definition ^ ".properties") "items" properties
    |> assoc (list_definition ^ ".properties.items")
  in
  required_member (list_definition ^ ".properties.items") "items" items_property
  |> required_definition_ref (list_definition ^ ".properties.items.items")

let definition_has_gvk definition schema expected =
  let fields = assoc definition schema in
  match member "x-kubernetes-group-version-kind" fields with
  | Some (`List entries) ->
      List.exists
        (fun entry ->
          let fields = assoc (definition ^ " GVK") entry in
          let actual =
            ( required_string (definition ^ " GVK") "group" fields,
              required_string (definition ^ " GVK") "version" fields,
              required_string (definition ^ " GVK") "kind" fields )
          in
          actual = expected)
        entries
  | Some _ ->
      fail "%s.x-kubernetes-group-version-kind must be an array" definition
  | None -> false

let resource_to_json resource =
  `Assoc
    [
      ("definition", `String resource.definition);
      ("module", `String resource.module_name);
      ("plural", `String resource.plural);
      ( "scope",
        `String
          (match resource.scope with
          | Namespaced -> "Namespaced"
          | Cluster -> "Cluster") );
    ]

let derive_stable_resources schema =
  let document = assoc "OpenAPI document" schema in
  let definitions = definitions_of_schema schema in
  let paths =
    required_member "OpenAPI document" "paths" document
    |> assoc "OpenAPI document.paths"
  in
  let path_map =
    List.fold_left
      (fun values (path, operations) -> String_map.add path operations values)
      String_map.empty paths
  in
  let resources =
    List.filter_map
      (fun (path, operations) ->
        let path_fields = assoc ("path " ^ path) operations in
        if
          String.contains path '{' || contains path "/watch/"
          || not (operation_is path "get" path_fields "list")
        then None
        else
          let get_fields =
            match operation path "get" path_fields with
            | Some fields -> fields
            | None -> assert false
          in
          let group, version, kind = operation_gvk (path ^ ".get") get_fields in
          if not (stable_version version) then None
          else
            let plural = path_last_segment "LIST operation" path in
            let base = path_without_last_segment "LIST operation" path in
            let watch_path = base ^ "/watch/" ^ plural in
            match String_map.find_opt watch_path path_map with
            | None -> None
            | Some watch_operations ->
                let watch_fields =
                  assoc ("path " ^ watch_path) watch_operations
                in
                if not (operation_is watch_path "get" watch_fields "watchlist")
                then None
                else
                  let watch_get =
                    match operation watch_path "get" watch_fields with
                    | Some fields -> fields
                    | None -> assert false
                  in
                  let watch_gvk =
                    operation_gvk (watch_path ^ ".get") watch_get
                  in
                  if watch_gvk <> (group, version, kind) then
                    fail "LIST %s and WATCH %s expose different GVKs" path
                      watch_path;
                  let definition =
                    list_item_definition definitions (path ^ ".get") get_fields
                  in
                  let definition_schema =
                    match String_map.find_opt definition definitions with
                    | Some value -> value
                    | None ->
                        fail "resource definition not found: %s" definition
                  in
                  if
                    not
                      (definition_has_gvk definition definition_schema
                         (group, version, kind))
                  then
                    fail
                      "LIST %s returns %s, which does not expose its operation \
                       GVK"
                      path definition;
                  let namespaced_path =
                    base ^ "/namespaces/{namespace}/" ^ plural
                  in
                  let scope =
                    match String_map.find_opt namespaced_path path_map with
                    | Some operations ->
                        let fields =
                          assoc ("path " ^ namespaced_path) operations
                        in
                        if operation_is namespaced_path "get" fields "list" then
                          Namespaced
                        else Cluster
                    | None -> Cluster
                  in
                  Some
                    ( {
                        definition;
                        module_name = resource_module group version;
                        plural;
                        scope;
                      },
                      kind ))
      paths
    |> List.sort (fun (left, left_kind) (right, right_kind) ->
        match String.compare left.module_name right.module_name with
        | 0 -> (
            match String.compare left_kind right_kind with
            | 0 -> String.compare left.definition right.definition
            | order -> order)
        | order -> order)
  in
  if resources = [] then fail "no stable list/watch resources were derived";
  let seen = Hashtbl.create (List.length resources) in
  List.iter
    (fun (resource, kind) ->
      let key = resource.module_name ^ "." ^ kind in
      if Hashtbl.mem seen key then fail "duplicate derived resource: %s" key;
      Hashtbl.add seen key ())
    resources;
  List.map fst resources

let derive_stable_manifest ~schema ~manifest:manifest_json =
  protect (fun () ->
      let manifest = parse_manifest manifest_json in
      let resources = derive_stable_resources schema in
      `Assoc
        [
          ("kubernetesVersion", `String manifest.kubernetes_version);
          ("source", `String manifest.source);
          ("sha256", `String manifest.sha256);
          ("resources", `List (List.map resource_to_json resources));
        ])

let derive_stable_manifest_with_metadata ~schema ~kubernetes_version ~source
    ~sha256 =
  protect (fun () ->
      if String.trim kubernetes_version = "" then
        fail "kubernetes version must not be empty";
      if String.trim source = "" then fail "source URL must not be empty";
      if String.length sha256 <> 64 then
        fail "source SHA-256 must contain 64 hexadecimal characters";
      if
        not
          (String.for_all
             (function
               | '0' .. '9' | 'a' .. 'f' -> true
               | _ -> false)
             sha256)
      then fail "source SHA-256 must be lowercase hexadecimal";
      let resources = derive_stable_resources schema in
      `Assoc
        [
          ("kubernetesVersion", `String kubernetes_version);
          ("source", `String source);
          ("sha256", `String sha256);
          ("resources", `List (List.map resource_to_json resources));
        ])

let rec references accumulator = function
  | `Assoc fields ->
      List.fold_left
        (fun accumulator (name, value) ->
          let accumulator =
            match (name, value) with
            | "$ref", `String reference -> (
                match definition_of_ref reference with
                | Some definition -> String_set.add definition accumulator
                | None ->
                    fail "unsupported non-definition reference: %s" reference)
            | _ -> accumulator
          in
          references accumulator value)
        accumulator fields
  | `List values -> List.fold_left references accumulator values
  | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ -> accumulator

let dependency_closure definitions roots =
  let rec visit visited name =
    if String_set.mem name visited then visited
    else
      let schema =
        match String_map.find_opt name definitions with
        | Some value -> value
        | None -> fail "definition not found: %s" name
      in
      let visited = String_set.add name visited in
      String_set.fold
        (fun dependency visited -> visit visited dependency)
        (references String_set.empty schema)
        visited
  in
  List.fold_left visit String_set.empty roots

let words value =
  let length = String.length value in
  let buffer = Buffer.create length in
  let parts = ref [] in
  let flush () =
    if Buffer.length buffer > 0 then (
      parts := Buffer.contents buffer :: !parts;
      Buffer.clear buffer)
  in
  let is_upper = function
    | 'A' .. 'Z' -> true
    | _ -> false
  in
  let is_lower = function
    | 'a' .. 'z' -> true
    | _ -> false
  in
  let is_digit = function
    | '0' .. '9' -> true
    | _ -> false
  in
  for index = 0 to length - 1 do
    let current = value.[index] in
    if not (is_upper current || is_lower current || is_digit current) then
      flush ()
    else
      let previous = if index = 0 then None else Some value.[index - 1] in
      let next = if index + 1 = length then None else Some value.[index + 1] in
      let boundary =
        is_upper current
        && Buffer.length buffer > 0
        &&
        match (previous, next) with
        | Some previous, _ when is_lower previous || is_digit previous -> true
        | Some previous, Some next when is_upper previous && is_lower next ->
            true
        | _ -> false
      in
      if boundary then flush ();
      Buffer.add_char buffer (Char.lowercase_ascii current)
  done;
  flush ();
  List.rev !parts

let reserved =
  [
    "and";
    "as";
    "assert";
    "begin";
    "class";
    "constraint";
    "continue";
    "do";
    "done";
    "downto";
    "else";
    "end";
    "effect";
    "exception";
    "external";
    "false";
    "for";
    "fun";
    "function";
    "functor";
    "if";
    "in";
    "include";
    "inherit";
    "initializer";
    "lazy";
    "land";
    "let";
    "lor";
    "lsl";
    "lsr";
    "lxor";
    "match";
    "method";
    "module";
    "mod";
    "mutable";
    "new";
    "nonrec";
    "object";
    "of";
    "open";
    "or";
    "private";
    "perform";
    "rec";
    "sig";
    "struct";
    "then";
    "to";
    "true";
    "try";
    "type";
    "val";
    "virtual";
    "when";
    "while";
    "with";
  ]

let identifier value =
  let value = String.concat "_" (words value) in
  let value = if value = "" then "field" else value in
  let value =
    match value.[0] with
    | '0' .. '9' -> "field_" ^ value
    | _ -> value
  in
  if List.mem value reserved then value ^ "_" else value

let type_name definition = identifier definition

type field = {
  json_name : string;
  ml_name : string;
  schema : Yojson.Safe.t;
  required : bool;
}

type shape = Record of field list | Alias of Yojson.Safe.t

let schema_fields context schema = assoc context schema

let shape_of_definition name schema =
  let fields = schema_fields name schema in
  let properties =
    match member "properties" fields with
    | None -> []
    | Some value -> assoc (name ^ ".properties") value
  in
  let has_additional_properties =
    member "additionalProperties" fields <> None
  in
  if properties <> [] && not has_additional_properties then
    let required =
      match member "required" fields with
      | None -> String_set.empty
      | Some value ->
          string_list (name ^ ".required") value
          |> List.fold_left
               (fun values field -> String_set.add field values)
               String_set.empty
    in
    let seen = Hashtbl.create (List.length properties) in
    let properties =
      List.map
        (fun (json_name, schema) ->
          let ml_name = identifier json_name in
          (match Hashtbl.find_opt seen ml_name with
          | Some other ->
              fail "%s has colliding OCaml fields %s and %s" name other
                json_name
          | None -> Hashtbl.add seen ml_name json_name);
          {
            json_name;
            ml_name;
            schema;
            required = String_set.mem json_name required;
          })
        properties
      |> List.sort (fun left right ->
          String.compare left.json_name right.json_name)
    in
    Record properties
  else Alias schema

let schema_ref schema =
  let fields = schema_fields "schema" schema in
  match member "$ref" fields with
  | Some (`String reference) -> (
      match definition_of_ref reference with
      | Some definition -> Some definition
      | None -> fail "unsupported reference: %s" reference)
  | Some _ -> fail "$ref must be a string"
  | None -> None

let rec schema_type schema =
  match schema_ref schema with
  | Some definition -> type_name definition
  | None -> (
      let fields = schema_fields "schema" schema in
      match optional_string "type" fields with
      | Some "string" -> (
          match optional_string "format" fields with
          | Some "int-or-string" -> "int_or_string"
          | _ -> "string")
      | Some "integer" -> (
          match optional_string "format" fields with
          | Some "int64" -> "int64"
          | Some "int32" -> "int32"
          | _ -> "int")
      | Some "number" -> "float"
      | Some "boolean" -> "bool"
      | Some "array" ->
          let items = required_member "array schema" "items" fields in
          Printf.sprintf "(%s) list" (schema_type items)
      | Some "object" -> (
          match member "additionalProperties" fields with
          | Some (`Bool true) -> "(string * Yojson.Safe.t) list"
          | Some (`Bool false) | None -> "Yojson.Safe.t"
          | Some additional ->
              Printf.sprintf "(string * (%s)) list" (schema_type additional))
      | None -> "Yojson.Safe.t"
      | Some value -> fail "unsupported OpenAPI type: %s" value)

let rec decoder schema argument =
  match schema_ref schema with
  | Some definition ->
      Printf.sprintf "%s_of_json %s" (type_name definition) argument
  | None -> (
      let fields = schema_fields "schema" schema in
      match optional_string "type" fields with
      | Some "string" -> (
          match optional_string "format" fields with
          | Some "int-or-string" -> "decode_int_or_string " ^ argument
          | _ -> "decode_string " ^ argument)
      | Some "integer" -> (
          match optional_string "format" fields with
          | Some "int64" -> "decode_int64 " ^ argument
          | Some "int32" -> "decode_int32 " ^ argument
          | _ -> "decode_int " ^ argument)
      | Some "number" -> "decode_float " ^ argument
      | Some "boolean" -> "decode_bool " ^ argument
      | Some "array" ->
          let items = required_member "array schema" "items" fields in
          Printf.sprintf "decode_list (fun json -> %s) %s"
            (decoder items "json") argument
      | Some "object" -> (
          match member "additionalProperties" fields with
          | Some (`Bool true) -> "decode_map (fun json -> Ok json) " ^ argument
          | Some (`Bool false) | None -> "Ok " ^ argument
          | Some additional ->
              Printf.sprintf "decode_map (fun json -> %s) %s"
                (decoder additional "json")
                argument)
      | None -> "Ok " ^ argument
      | Some value -> fail "unsupported OpenAPI type in decoder: %s" value)

let rec encoder schema argument =
  match schema_ref schema with
  | Some definition ->
      Printf.sprintf "%s_to_json %s" (type_name definition) argument
  | None -> (
      let fields = schema_fields "schema" schema in
      match optional_string "type" fields with
      | Some "string" -> (
          match optional_string "format" fields with
          | Some "int-or-string" -> "encode_int_or_string " ^ argument
          | _ -> "`String " ^ argument)
      | Some "integer" -> (
          match optional_string "format" fields with
          | Some "int64" -> "`Intlit (Int64.to_string " ^ argument ^ ")"
          | Some "int32" -> "`Intlit (Int32.to_string " ^ argument ^ ")"
          | _ -> "`Int " ^ argument)
      | Some "number" -> "`Float " ^ argument
      | Some "boolean" -> "`Bool " ^ argument
      | Some "array" ->
          let items = required_member "array schema" "items" fields in
          Printf.sprintf "`List (List.map (fun value -> %s) %s)"
            (encoder items "value") argument
      | Some "object" -> (
          match member "additionalProperties" fields with
          | Some (`Bool true) -> "`Assoc " ^ argument
          | Some (`Bool false) | None -> argument
          | Some additional ->
              Printf.sprintf
                "`Assoc (List.map (fun (name, value) -> (name, %s)) %s)"
                (encoder additional "value")
                argument)
      | None -> argument
      | Some value -> fail "unsupported OpenAPI type in encoder: %s" value)

let buffer_line buffer indentation format =
  Printf.ksprintf
    (fun line ->
      Buffer.add_string buffer (String.make indentation ' ');
      Buffer.add_string buffer line;
      Buffer.add_char buffer '\n')
    format

let emit_type_group buffer definitions shapes =
  List.iteri
    (fun index definition ->
      let prefix = if index = 0 then "type" else "and" in
      let name = type_name definition in
      match String_map.find definition shapes with
      | Alias schema ->
          buffer_line buffer 0 "%s %s = %s" prefix name (schema_type schema)
      | Record fields ->
          buffer_line buffer 0 "%s %s = {" prefix name;
          List.iter
            (fun field ->
              let field_type = schema_type field.schema in
              buffer_line buffer 2 "%s : %s%s;" field.ml_name field_type
                (if field.required then "" else " option"))
            fields;
          buffer_line buffer 0 "}")
    definitions;
  Buffer.add_char buffer '\n'

let maker_arguments fields =
  fields
  |> List.map (fun field ->
      let marker = if field.required then "" else "?" in
      Printf.sprintf "%s%s:%s" marker field.ml_name (schema_type field.schema))
  |> String.concat " -> "

let emit_maker_signature buffer name fields =
  let arguments = maker_arguments fields in
  let arguments = if arguments = "" then "unit" else arguments ^ " -> unit" in
  buffer_line buffer 0 "val make_%s : %s -> %s" name arguments name

let emit_maker_implementation buffer name fields =
  let parameters =
    fields
    |> List.map (fun field ->
        (if field.required then "~" else "?") ^ field.ml_name)
    |> String.concat " "
  in
  let parameters = if parameters = "" then "" else parameters ^ " " in
  buffer_line buffer 0 "let make_%s %s() : %s =" name parameters name;
  buffer_line buffer 2 "{";
  List.iter (fun field -> buffer_line buffer 4 "%s;" field.ml_name) fields;
  buffer_line buffer 2 "}";
  Buffer.add_char buffer '\n'

let emit_makers buffer definitions shapes ~interface =
  List.iter
    (fun definition ->
      match String_map.find definition shapes with
      | Alias _ -> ()
      | Record fields ->
          let name = type_name definition in
          if interface then emit_maker_signature buffer name fields
          else emit_maker_implementation buffer name fields)
    definitions;
  if interface then Buffer.add_char buffer '\n'

let emit_helpers buffer =
  Buffer.add_string buffer
    {|let ( let* ) result fn =
  match result with Ok value -> fn value | Error _ as error -> error

let with_context context = function
  | Ok value -> Ok value
  | Error message -> Error (context ^ ": " ^ message)

let decode_string = function
  | `String value -> Ok value
  | _ -> Error "expected string"

let decode_bool = function
  | `Bool value -> Ok value
  | _ -> Error "expected boolean"

let integer_literal = function
  | `Int value -> Some (string_of_int value)
  | `Intlit value -> Some value
  | _ -> None

let decode_int json =
  match integer_literal json with
  | Some value -> (
      match int_of_string_opt value with
      | Some value -> Ok value
      | None -> Error "integer is outside the OCaml int range")
  | None -> Error "expected integer"

let decode_int32 json =
  match integer_literal json with
  | Some value -> (
      try Ok (Int32.of_string value)
      with Failure _ -> Error "integer is outside the int32 range")
  | None -> Error "expected int32"

let decode_int64 json =
  match integer_literal json with
  | Some value -> (
      try Ok (Int64.of_string value)
      with Failure _ -> Error "integer is outside the int64 range")
  | None -> Error "expected int64"

let decode_float = function
  | `Float value -> Ok value
  | `Int value -> Ok (float_of_int value)
  | `Intlit value -> (
      try Ok (float_of_string value) with Failure _ -> Error "invalid number")
  | _ -> Error "expected number"

let decode_int_or_string = function
  | `String value -> Ok (`String value)
  | (`Int _ | `Intlit _) as json ->
      let* value = decode_int32 json in
      Ok (`Int value)
  | _ -> Error "expected int32 or string"

let encode_int_or_string = function
  | `String value -> `String value
  | `Int value -> `Intlit (Int32.to_string value)

let decode_list decode = function
  | `List values ->
      let rec loop index accumulator = function
        | [] -> Ok (List.rev accumulator)
        | value :: rest ->
            let* value = with_context (Printf.sprintf "item %d" index) (decode value) in
            loop (index + 1) (value :: accumulator) rest
      in
      loop 0 [] values
  | _ -> Error "expected array"

let decode_map decode = function
  | `Assoc fields ->
      let rec loop accumulator = function
        | [] -> Ok (List.rev accumulator)
        | (name, json) :: rest ->
            let* value = with_context name (decode json) in
            loop ((name, value) :: accumulator) rest
      in
      loop [] fields
  | _ -> Error "expected object"

let optional_field name decode fields =
  match List.assoc_opt name fields with
  | None | Some `Null -> Ok None
  | Some json ->
      let* value = with_context name (decode json) in
      Ok (Some value)

let required_field name decode fields =
  match List.assoc_opt name fields with
  | None -> Error (name ^ ": required field is missing")
  | Some `Null -> Error (name ^ ": required field is null")
  | Some json -> with_context name (decode json)

let optional_json name encode = function
  | None -> None
  | Some value -> Some (name, encode value)

let () =
  ignore decode_int;
  ignore decode_float

let metadata_of_resource_json json =
  match Kube.Core.object_meta_of_json json with
  | Ok metadata -> metadata
  | Error _ ->
      let json =
        match json with
        | `Assoc fields ->
            let metadata =
              match List.assoc_opt "metadata" fields with
              | Some (`Assoc metadata) ->
                  if List.mem_assoc "name" metadata then `Assoc metadata
                  else `Assoc (("name", `String "") :: metadata)
              | _ -> `Assoc [ ("name", `String "") ]
            in
            `Assoc (("metadata", metadata) :: List.remove_assoc "metadata" fields)
        | _ -> `Assoc [ ("metadata", `Assoc [ ("name", `String "") ]) ]
      in
      match Kube.Core.object_meta_of_json json with
      | Ok metadata -> metadata
      | Error message -> invalid_arg ("generated resource metadata: " ^ message)

|}

let emit_decoder buffer definition shape prefix =
  let name = type_name definition in
  match shape with
  | Alias schema ->
      buffer_line buffer 0
        "%s %s_of_json (json : Yojson.Safe.t) : (%s, string) result = %s" prefix
        name name (decoder schema "json")
  | Record fields ->
      buffer_line buffer 0
        "%s %s_of_json (json : Yojson.Safe.t) : (%s, string) result =" prefix
        name name;
      buffer_line buffer 2 "match json with";
      buffer_line buffer 2 "| `Assoc fields ->";
      List.iter
        (fun field ->
          let decode =
            Printf.sprintf "(fun json -> %s)" (decoder field.schema "json")
          in
          buffer_line buffer 6 "let* %s = %s_field %S %s fields in"
            field.ml_name
            (if field.required then "required" else "optional")
            field.json_name decode)
        fields;
      buffer_line buffer 6 "Ok";
      buffer_line buffer 8 "({";
      List.iter (fun field -> buffer_line buffer 10 "%s;" field.ml_name) fields;
      buffer_line buffer 8 "} : %s)" name;
      buffer_line buffer 2 "| _ -> Error %S" (definition ^ " must be an object")

let emit_encoder buffer definition shape prefix =
  let name = type_name definition in
  match shape with
  | Alias schema ->
      buffer_line buffer 0 "%s %s_to_json (value : %s) : Yojson.Safe.t = %s"
        prefix name name (encoder schema "value")
  | Record fields ->
      buffer_line buffer 0 "%s %s_to_json (value : %s) : Yojson.Safe.t =" prefix
        name name;
      buffer_line buffer 2 "`Assoc";
      buffer_line buffer 4 "(List.filter_map Fun.id";
      buffer_line buffer 6 "[";
      List.iter
        (fun field ->
          let encode =
            Printf.sprintf "(fun value -> %s)" (encoder field.schema "value")
          in
          if field.required then
            buffer_line buffer 8 "Some (%S, %s);" field.json_name
              (encoder field.schema ("value." ^ field.ml_name))
          else
            buffer_line buffer 8 "optional_json %S %s value.%s;" field.json_name
              encode field.ml_name)
        fields;
      buffer_line buffer 6 "])"

let emit_codecs buffer definitions shapes =
  let first = ref true in
  List.iter
    (fun definition ->
      let shape = String_map.find definition shapes in
      let prefix () =
        if !first then (
          first := false;
          "let rec")
        else "and"
      in
      emit_decoder buffer definition shape (prefix ());
      emit_encoder buffer definition shape (prefix ()))
    definitions;
  Buffer.add_char buffer '\n'

let gvk definition schema =
  let fields = schema_fields definition schema in
  let entries =
    match member "x-kubernetes-group-version-kind" fields with
    | Some value -> list (definition ^ ".x-kubernetes-group-version-kind") value
    | None -> fail "%s has no x-kubernetes-group-version-kind" definition
  in
  match entries with
  | [] -> fail "%s has an empty x-kubernetes-group-version-kind" definition
  | value :: _ ->
      let fields = assoc (definition ^ " GVK") value in
      ( required_string (definition ^ " GVK") "group" fields,
        required_string (definition ^ " GVK") "version" fields,
        required_string (definition ^ " GVK") "kind" fields )

let grouped_resources definitions resources =
  let sorted =
    List.map
      (fun resource ->
        let schema =
          match String_map.find_opt resource.definition definitions with
          | Some schema -> schema
          | None ->
              fail "selected resource definition not found: %s"
                resource.definition
        in
        let group, version, kind = gvk resource.definition schema in
        (resource, group, version, kind))
      resources
    |> List.sort (fun (left, _, _, left_kind) (right, _, _, right_kind) ->
        match String.compare left.module_name right.module_name with
        | 0 -> String.compare left_kind right_kind
        | value -> value)
  in
  let rec collect current_name current_values groups = function
    | [] ->
        List.rev
          (match current_name with
          | None -> groups
          | Some name -> (name, List.rev current_values) :: groups)
    | ((resource, _, _, _) as value) :: rest -> (
        match current_name with
        | Some name when name = resource.module_name ->
            collect current_name (value :: current_values) groups rest
        | Some name ->
            collect (Some resource.module_name) [ value ]
              ((name, List.rev current_values) :: groups)
              rest
        | None -> collect (Some resource.module_name) [ value ] groups rest)
  in
  collect None [] [] sorted

let maker_type fields result =
  let arguments = maker_arguments fields in
  if arguments = "" then "unit -> " ^ result
  else arguments ^ " -> unit -> " ^ result

let emit_codec_module buffer shapes ~interface ~indent ~module_name definition =
  let name = type_name definition in
  let shape =
    match String_map.find_opt definition shapes with
    | Some shape -> shape
    | None ->
        fail "support type is missing from dependency closure: %s" definition
  in
  buffer_line buffer indent "module %s %s" module_name
    (if interface then ": sig" else "= struct");
  if interface then (
    buffer_line buffer (indent + 2) "type t = %s" name;
    (match shape with
    | Record fields ->
        buffer_line buffer (indent + 2) "val make : %s" (maker_type fields "t")
    | Alias _ -> ());
    buffer_line buffer (indent + 2)
      "val of_json : Yojson.Safe.t -> (t, string) result";
    buffer_line buffer (indent + 2) "val to_json : t -> Yojson.Safe.t";
    buffer_line buffer indent "end")
  else (
    buffer_line buffer (indent + 2) "type nonrec t = %s" name;
    (match shape with
    | Record _ -> buffer_line buffer (indent + 2) "let make = make_%s" name
    | Alias _ -> ());
    buffer_line buffer (indent + 2) "let of_json = %s_of_json" name;
    buffer_line buffer (indent + 2) "let to_json = %s_to_json" name;
    buffer_line buffer indent "end")

let emit_support_modules buffer shapes ~interface =
  let metadata_types =
    [
      ("Condition", "io.k8s.apimachinery.pkg.apis.meta.v1.Condition");
      ("LabelSelector", "io.k8s.apimachinery.pkg.apis.meta.v1.LabelSelector");
      ( "LabelSelectorRequirement",
        "io.k8s.apimachinery.pkg.apis.meta.v1.LabelSelectorRequirement" );
      ("ObjectMeta", "io.k8s.apimachinery.pkg.apis.meta.v1.ObjectMeta");
      ("OwnerReference", "io.k8s.apimachinery.pkg.apis.meta.v1.OwnerReference");
    ]
    |> List.filter (fun (_, definition) -> String_map.mem definition shapes)
  in
  if metadata_types <> [] then (
    buffer_line buffer 0 "module Meta_v1 %s"
      (if interface then ": sig" else "= struct");
    List.iter
      (fun (module_name, definition) ->
        emit_codec_module buffer shapes ~interface ~indent:2 ~module_name
          definition)
      metadata_types;
    buffer_line buffer 0 "end";
    Buffer.add_char buffer '\n');
  let int_or_string_definition =
    "io.k8s.apimachinery.pkg.util.intstr.IntOrString"
  in
  if String_map.mem int_or_string_definition shapes then (
    let int_or_string_name = type_name int_or_string_definition in
    buffer_line buffer 0 "module Int_or_string %s"
      (if interface then ": sig" else "= struct");
    if interface then (
      buffer_line buffer 2 "type t = int_or_string";
      buffer_line buffer 2 "val of_int32 : int32 -> t";
      buffer_line buffer 2 "val of_string : string -> t";
      buffer_line buffer 2 "val of_json : Yojson.Safe.t -> (t, string) result";
      buffer_line buffer 2 "val to_json : t -> Yojson.Safe.t";
      buffer_line buffer 0 "end")
    else (
      buffer_line buffer 2 "type nonrec t = int_or_string";
      buffer_line buffer 2 "let of_int32 value = `Int value";
      buffer_line buffer 2 "let of_string value = `String value";
      buffer_line buffer 2 "let of_json = %s_of_json" int_or_string_name;
      buffer_line buffer 2 "let to_json = %s_to_json" int_or_string_name;
      buffer_line buffer 0 "end");
    Buffer.add_char buffer '\n');
  let quantity_definition = "io.k8s.apimachinery.pkg.api.resource.Quantity" in
  if String_map.mem quantity_definition shapes then (
    let quantity_name = type_name quantity_definition in
    buffer_line buffer 0 "module Quantity %s"
      (if interface then ": sig" else "= struct");
    if interface then (
      buffer_line buffer 2 "type t = string";
      buffer_line buffer 2 "val of_string : string -> t";
      buffer_line buffer 2 "val to_string : t -> string";
      buffer_line buffer 2 "val of_json : Yojson.Safe.t -> (t, string) result";
      buffer_line buffer 2 "val to_json : t -> Yojson.Safe.t";
      buffer_line buffer 0 "end")
    else (
      buffer_line buffer 2 "type t = string";
      buffer_line buffer 2 "let of_string value = value";
      buffer_line buffer 2 "let to_string value = value";
      buffer_line buffer 2 "let of_json = %s_of_json" quantity_name;
      buffer_line buffer 2 "let to_json = %s_to_json" quantity_name;
      buffer_line buffer 0 "end");
    Buffer.add_char buffer '\n')

let emit_resource_modules buffer definitions shapes resources ~interface =
  grouped_resources definitions resources
  |> List.iter (fun (group_module, resources) ->
      buffer_line buffer 0 "module %s %s" group_module
        (if interface then ": sig" else "= struct");
      List.iter
        (fun (resource, group, version, kind) ->
          let type_name = type_name resource.definition in
          let fields =
            match String_map.find resource.definition shapes with
            | Record fields -> fields
            | Alias _ -> fail "resource %s is not an object" resource.definition
          in
          if interface then (
            buffer_line buffer 2 "module %s : sig" kind;
            buffer_line buffer 4 "type t = %s" type_name;
            buffer_line buffer 4 "val make : %s" (maker_type fields "t");
            buffer_line buffer 4 "include Kube.Core.Resource with type t := t";
            buffer_line buffer 2 "end")
          else (
            buffer_line buffer 2 "module %s = struct" kind;
            buffer_line buffer 4 "type nonrec t = %s" type_name;
            buffer_line buffer 4 "let make = make_%s" type_name;
            buffer_line buffer 4 "let api =";
            buffer_line buffer 6 "{";
            buffer_line buffer 8 "Kube.Core.group = %S;" group;
            buffer_line buffer 8 "version = %S;" version;
            buffer_line buffer 8 "kind = %S;" kind;
            buffer_line buffer 8 "plural = %S;" resource.plural;
            buffer_line buffer 8 "scope = Kube.Core.%s;"
              (match resource.scope with
              | Namespaced -> "Namespaced"
              | Cluster -> "Cluster");
            buffer_line buffer 6 "}";
            buffer_line buffer 4 "let metadata value =";
            buffer_line buffer 6 "metadata_of_resource_json (%s_to_json value)"
              type_name;
            buffer_line buffer 4 "let of_json = %s_of_json" type_name;
            buffer_line buffer 4 "let to_json = %s_to_json" type_name;
            buffer_line buffer 2 "end"))
        resources;
      buffer_line buffer 0 "end";
      Buffer.add_char buffer '\n')

let emit_resource_registry buffer definitions resources ~interface =
  if interface then
    buffer_line buffer 0 "val all_resources : Kube.Core.api list"
  else (
    buffer_line buffer 0 "let all_resources =";
    buffer_line buffer 2 "[";
    grouped_resources definitions resources
    |> List.iter (fun (group_module, resources) ->
        List.iter
          (fun (_, _, _, kind) ->
            buffer_line buffer 4 "%s.%s.api;" group_module kind)
          resources);
    buffer_line buffer 2 "]")

let header manifest definition_count =
  Printf.sprintf
    "(** Generated by ocaml-k8s from Kubernetes %s OpenAPI v2.\n\
    \    Source: %s\n\
    \    Upstream SHA-256: %s\n\
    \    Definitions in dependency closure: %d.\n\
    \    Do not edit by hand. *)\n\n"
    manifest.kubernetes_version manifest.source manifest.sha256 definition_count

let render manifest definition_map selected =
  let definition_names = String_set.elements selected in
  let names = Hashtbl.create (List.length definition_names) in
  List.iter
    (fun definition ->
      let name = type_name definition in
      match Hashtbl.find_opt names name with
      | Some other ->
          fail "definitions %s and %s map to the same OCaml type %s" other
            definition name
      | None -> Hashtbl.add names name definition)
    definition_names;
  let shapes =
    List.fold_left
      (fun values definition ->
        let schema = String_map.find definition definition_map in
        String_map.add definition (shape_of_definition definition schema) values)
      String_map.empty definition_names
  in
  let implementation = Buffer.create 1_000_000 in
  Buffer.add_string implementation
    (header manifest (List.length definition_names));
  Buffer.add_string implementation
    "type int_or_string = [ `Int of int32 | `String of string ]\n\n";
  emit_type_group implementation definition_names shapes;
  emit_makers implementation definition_names shapes ~interface:false;
  emit_helpers implementation;
  emit_codecs implementation definition_names shapes;
  emit_support_modules implementation shapes ~interface:false;
  emit_resource_modules implementation definition_map shapes manifest.resources
    ~interface:false;
  emit_resource_registry implementation definition_map manifest.resources
    ~interface:false;
  let interface = Buffer.create 500_000 in
  Buffer.add_string interface (header manifest (List.length definition_names));
  Buffer.add_string interface
    "type int_or_string = [ `Int of int32 | `String of string ]\n\n";
  emit_type_group interface definition_names shapes;
  emit_makers interface definition_names shapes ~interface:true;
  List.iter
    (fun definition ->
      let name = type_name definition in
      buffer_line interface 0
        "val %s_of_json : Yojson.Safe.t -> (%s, string) result" name name;
      buffer_line interface 0 "val %s_to_json : %s -> Yojson.Safe.t" name name)
    definition_names;
  Buffer.add_char interface '\n';
  emit_support_modules interface shapes ~interface:true;
  emit_resource_modules interface definition_map shapes manifest.resources
    ~interface:true;
  emit_resource_registry interface definition_map manifest.resources
    ~interface:true;
  (Buffer.contents implementation, Buffer.contents interface, definition_names)

let pruned_schema manifest definitions selected_names =
  let selected =
    List.map
      (fun name -> (name, String_map.find name definitions))
      selected_names
  in
  `Assoc
    [
      ("swagger", `String "2.0");
      ( "info",
        `Assoc
          [
            ("title", `String "ocaml-k8s generated API dependency closure");
            ("version", `String manifest.kubernetes_version);
          ] );
      ("x-ocaml-k8s-upstream-source", `String manifest.source);
      ("x-ocaml-k8s-upstream-sha256", `String manifest.sha256);
      ("definitions", `Assoc selected);
    ]
  |> Yojson.Safe.pretty_to_string
  |> fun value -> value ^ "\n"

let generate ~schema ~manifest:manifest_json =
  protect (fun () ->
      let manifest = parse_manifest manifest_json in
      let definitions = definitions_of_schema schema in
      let roots =
        List.map (fun resource -> resource.definition) manifest.resources
      in
      let selected = dependency_closure definitions roots in
      let implementation, interface, selected_names =
        render manifest definitions selected
      in
      {
        implementation;
        interface;
        pruned_schema = pruned_schema manifest definitions selected_names;
        definition_count = List.length selected_names;
      })
