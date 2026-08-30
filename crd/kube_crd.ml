module Schema = struct
  type t =
    | String of {
        format : string option;
        enum : string list;
        min_length : int option;
        max_length : int option;
        pattern : string option;
      }
    | Integer of {
        format : [ `Int32 | `Int64 ] option;
        minimum : int option;
        maximum : int option;
      }
    | Number of { format : [ `Float | `Double ] option }
    | Boolean
    | Array of {
        items : t;
        min_items : int option;
        max_items : int option;
        unique_items : bool;
      }
    | Object of {
        properties : (string * t) list;
        required : string list;
        additional_properties : t option;
        preserve_unknown_fields : bool;
      }
    | One_of of t list
    | Int_or_string
    | Preserve_unknown
    | Raw of Yojson.Safe.t
    | Decorated of {
        schema : t;
        description : string option;
        default : Yojson.Safe.t option;
        nullable : bool;
      }

  let non_negative name = function
    | Some value when value < 0 ->
        invalid_arg ("Kube_crd.Schema." ^ name ^ " must not be negative")
    | value -> value

  let string ?format ?(enum = []) ?min_length ?max_length ?pattern () =
    let min_length = non_negative "min_length" min_length in
    let max_length = non_negative "max_length" max_length in
    (match (min_length, max_length) with
    | Some minimum, Some maximum when minimum > maximum ->
        invalid_arg "Kube_crd.Schema.string: min_length exceeds max_length"
    | _ -> ());
    String { format; enum; min_length; max_length; pattern }

  let integer ?format ?minimum ?maximum () =
    (match (minimum, maximum) with
    | Some minimum, Some maximum when minimum > maximum ->
        invalid_arg "Kube_crd.Schema.integer: minimum exceeds maximum"
    | _ -> ());
    Integer { format; minimum; maximum }

  let number ?format () = Number { format }
  let boolean () = Boolean

  let array ?min_items ?max_items ?(unique_items = false) items =
    let min_items = non_negative "min_items" min_items in
    let max_items = non_negative "max_items" max_items in
    (match (min_items, max_items) with
    | Some minimum, Some maximum when minimum > maximum ->
        invalid_arg "Kube_crd.Schema.array: min_items exceeds max_items"
    | _ -> ());
    Array { items; min_items; max_items; unique_items }

  let object_ ?(required = []) ?additional_properties
      ?(preserve_unknown_fields = false) properties =
    Object
      { properties; required; additional_properties; preserve_unknown_fields }

  let map values = object_ ~additional_properties:values []
  let one_of schemas = One_of schemas
  let int_or_string () = Int_or_string
  let preserve_unknown () = Preserve_unknown
  let raw value = Raw value

  let describe description = function
    | Decorated decoration ->
        Decorated { decoration with description = Some description }
    | schema ->
        Decorated
          {
            schema;
            description = Some description;
            default = None;
            nullable = false;
          }

  let with_default default = function
    | Decorated decoration ->
        Decorated { decoration with default = Some default }
    | schema ->
        Decorated
          {
            schema;
            description = None;
            default = Some default;
            nullable = false;
          }

  let nullable = function
    | Decorated decoration -> Decorated { decoration with nullable = true }
    | schema ->
        Decorated
          { schema; description = None; default = None; nullable = true }

  let optional name value =
    match value with
    | None -> []
    | Some value -> [ (name, value) ]

  let add_fields fields = function
    | `Assoc existing -> `Assoc (existing @ fields)
    | value ->
        `Assoc
          [
            ("allOf", `List [ value ]);
            ("x-ocaml-k8s-decoration-error", `Bool true);
          ]

  let rec to_json = function
    | String { format; enum; min_length; max_length; pattern } ->
        `Assoc
          ([ ("type", `String "string") ]
          @ optional "format" (Option.map (fun value -> `String value) format)
          @ (if enum = [] then []
             else
               [ ("enum", `List (List.map (fun value -> `String value) enum)) ])
          @ optional "minLength"
              (Option.map (fun value -> `Int value) min_length)
          @ optional "maxLength"
              (Option.map (fun value -> `Int value) max_length)
          @ optional "pattern" (Option.map (fun value -> `String value) pattern)
          )
    | Integer { format; minimum; maximum } ->
        let format =
          Option.map
            (function
              | `Int32 -> `String "int32"
              | `Int64 -> `String "int64")
            format
        in
        `Assoc
          ([ ("type", `String "integer") ]
          @ optional "format" format
          @ optional "minimum" (Option.map (fun value -> `Int value) minimum)
          @ optional "maximum" (Option.map (fun value -> `Int value) maximum))
    | Number { format } ->
        let format =
          Option.map
            (function
              | `Float -> `String "float"
              | `Double -> `String "double")
            format
        in
        `Assoc ([ ("type", `String "number") ] @ optional "format" format)
    | Boolean -> `Assoc [ ("type", `String "boolean") ]
    | Array { items; min_items; max_items; unique_items } ->
        `Assoc
          ([ ("type", `String "array"); ("items", to_json items) ]
          @ optional "minItems" (Option.map (fun value -> `Int value) min_items)
          @ optional "maxItems" (Option.map (fun value -> `Int value) max_items)
          @ if unique_items then [ ("uniqueItems", `Bool true) ] else [])
    | Object
        { properties; required; additional_properties; preserve_unknown_fields }
      ->
        `Assoc
          ([ ("type", `String "object") ]
          @ (if required = [] then []
             else
               [
                 ( "required",
                   `List (List.map (fun value -> `String value) required) );
               ])
          @ (if properties = [] then []
             else
               [
                 ( "properties",
                   `Assoc
                     (List.map
                        (fun (name, schema) -> (name, to_json schema))
                        properties) );
               ])
          @ optional "additionalProperties"
              (Option.map to_json additional_properties)
          @
          if preserve_unknown_fields then
            [ ("x-kubernetes-preserve-unknown-fields", `Bool true) ]
          else [])
    | One_of schemas -> `Assoc [ ("oneOf", `List (List.map to_json schemas)) ]
    | Int_or_string ->
        `Assoc
          [
            ( "anyOf",
              `List
                [
                  `Assoc [ ("type", `String "integer") ];
                  `Assoc [ ("type", `String "string") ];
                ] );
            ("x-kubernetes-int-or-string", `Bool true);
          ]
    | Preserve_unknown ->
        `Assoc
          [
            ("type", `String "object");
            ("x-kubernetes-preserve-unknown-fields", `Bool true);
          ]
    | Raw value -> value
    | Decorated { schema; description; default; nullable } ->
        to_json schema
        |> add_fields
             (optional "description"
                (Option.map (fun value -> `String value) description)
             @ optional "default" default
             @ if nullable then [ ("nullable", `Bool true) ] else [])

  let duplicates values =
    let sorted = List.sort String.compare values in
    let rec loop result = function
      | left :: (right :: _ as rest) when left = right ->
          loop (left :: result) rest
      | _ :: rest -> loop result rest
      | [] -> List.sort_uniq String.compare result
    in
    loop [] sorted

  let validate schema =
    let rec visit path errors = function
      | String { enum; _ } ->
          List.fold_left
            (fun errors duplicate ->
              (path ^ ": duplicate enum value " ^ duplicate) :: errors)
            errors (duplicates enum)
      | Integer _ | Number _ | Boolean | Int_or_string | Preserve_unknown ->
          errors
      | Array { items; _ } -> visit (path ^ ".items") errors items
      | Object { properties; required; additional_properties; _ } ->
          let names = List.map fst properties in
          let errors =
            List.fold_left
              (fun errors duplicate ->
                (path ^ ": duplicate property " ^ duplicate) :: errors)
              errors (duplicates names)
          in
          let errors =
            List.fold_left
              (fun errors duplicate ->
                (path ^ ": duplicate required field " ^ duplicate) :: errors)
              errors (duplicates required)
          in
          let errors =
            List.fold_left
              (fun errors name ->
                if List.mem name names then errors
                else
                  (path ^ ": required field has no schema: " ^ name) :: errors)
              errors required
          in
          let errors =
            match (properties, additional_properties) with
            | _ :: _, Some _ ->
                (path
               ^ ": structural schemas cannot combine properties with typed \
                  additionalProperties")
                :: errors
            | _ -> errors
          in
          let errors =
            List.fold_left
              (fun errors (name, schema) ->
                visit (path ^ ".properties." ^ name) errors schema)
              errors properties
          in
          Option.fold ~none:errors
            ~some:(visit (path ^ ".additionalProperties") errors)
            additional_properties
      | One_of schemas ->
          let errors =
            if List.length schemas < 2 then
              (path ^ ": oneOf requires at least two alternatives") :: errors
            else errors
          in
          List.mapi (fun index schema -> (index, schema)) schemas
          |> List.fold_left
               (fun errors (index, schema) ->
                 visit (Printf.sprintf "%s.oneOf[%d]" path index) errors schema)
               errors
      | Raw (`Assoc _) -> errors
      | Raw _ -> (path ^ ": raw schema must be a JSON object") :: errors
      | Decorated { schema; _ } -> visit path errors schema
    in
    match List.rev (visit "$" [] schema) with
    | [] -> Ok ()
    | errors -> Error errors

  let rec is_object = function
    | Object _ | Preserve_unknown -> true
    | Decorated { schema; _ } -> is_object schema
    | Raw (`Assoc fields) -> (
        match List.assoc_opt "type" fields with
        | Some (`String "object") -> true
        | _ -> false)
    | _ -> false
end

module Yaml = struct
  let spaces count = String.make count ' '

  let plain_string value =
    let reserved = [ "null"; "true"; "false"; "yes"; "no"; "on"; "off"; "~" ] in
    value <> ""
    && (not (List.mem (String.lowercase_ascii value) reserved))
    && String.for_all
         (function
           | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' | '.' | '/' ->
               true
           | _ -> false)
         value
    &&
    match value.[0] with
    | '0' .. '9' | '-' | '.' -> false
    | _ -> true

  let string value =
    if plain_string value then value else Yojson.Safe.to_string (`String value)

  let scalar = function
    | `Null -> Some "null"
    | `Bool value -> Some (string_of_bool value)
    | `Int value -> Some (string_of_int value)
    | `Intlit value -> Some value
    | `Float value -> Some (Yojson.Safe.to_string (`Float value))
    | `String value -> Some (string value)
    | `Assoc _ | `List _ | `Tuple _ | `Variant _ -> None

  let rec emit buffer indent = function
    | (`Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _) as value ->
        Buffer.add_string buffer (Option.get (scalar value));
        Buffer.add_char buffer '\n'
    | `Assoc [] ->
        Buffer.add_string buffer (spaces indent);
        Buffer.add_string buffer "{}\n"
    | `List [] ->
        Buffer.add_string buffer (spaces indent);
        Buffer.add_string buffer "[]\n"
    | `Assoc fields ->
        List.iter
          (fun (name, value) ->
            Buffer.add_string buffer (spaces indent);
            Buffer.add_string buffer (string name);
            Buffer.add_char buffer ':';
            match scalar value with
            | Some value ->
                Buffer.add_char buffer ' ';
                Buffer.add_string buffer value;
                Buffer.add_char buffer '\n'
            | None ->
                Buffer.add_char buffer '\n';
                emit buffer (indent + 2) value)
          fields
    | `List values ->
        List.iter
          (fun value ->
            Buffer.add_string buffer (spaces indent);
            Buffer.add_char buffer '-';
            match scalar value with
            | Some value ->
                Buffer.add_char buffer ' ';
                Buffer.add_string buffer value;
                Buffer.add_char buffer '\n'
            | None ->
                Buffer.add_char buffer '\n';
                emit buffer (indent + 2) value)
          values
    | `Tuple values -> emit buffer indent (`List values)
    | `Variant (tag, None) -> emit buffer indent (`String tag)
    | `Variant (tag, Some value) -> emit buffer indent (`Assoc [ (tag, value) ])

  let encode value =
    let buffer = Buffer.create 4096 in
    emit buffer 0 value;
    Buffer.contents buffer
end

module Custom_resource_definition = struct
  type scale = {
    spec_replicas_path : string;
    status_replicas_path : string;
    label_selector_path : string option;
  }

  type printer_column_type = [ `Integer | `Number | `String | `Boolean | `Date ]

  type printer_column = {
    name : string;
    type_ : printer_column_type;
    json_path : string;
    format : string option;
    description : string option;
    priority : int option;
  }

  type version = {
    name : string;
    served : bool;
    storage : bool;
    status : bool;
    scale : scale option;
    printer_columns : printer_column list;
    schema : Schema.t;
  }

  type t = {
    group : string;
    kind : string;
    plural : string;
    singular : string;
    list_kind : string;
    short_names : string list;
    categories : string list;
    scope : Kube.Core.scope;
    versions : version list;
  }

  let scale ?label_selector_path ~spec_replicas_path ~status_replicas_path () =
    { spec_replicas_path; status_replicas_path; label_selector_path }

  let printer_column ?format ?description ?priority ~name ~type_ ~json_path () =
    { name; type_; json_path; format; description; priority }

  let version ?(served = true) ?(storage = false) ?(status = false) ?scale
      ?(printer_columns = []) ~name ~schema () =
    { name; served; storage; status; scale; printer_columns; schema }

  let valid_dns_label value =
    let length = String.length value in
    length > 0 && length <= 63
    &&
    let alphanumeric = function
      | 'a' .. 'z' | '0' .. '9' -> true
      | _ -> false
    in
    alphanumeric value.[0]
    && alphanumeric value.[length - 1]
    && String.for_all
         (function
           | 'a' .. 'z' | '0' .. '9' | '-' -> true
           | _ -> false)
         value

  let valid_group value =
    value <> ""
    && String.length value <= 253
    && String.split_on_char '.' value |> List.for_all valid_dns_label

  let valid_kind value =
    value <> ""
    &&
    match value.[0] with
    | 'A' .. 'Z' ->
        String.for_all
          (function
            | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
            | _ -> false)
          value
    | _ -> false

  let valid_version value =
    let length = String.length value in
    let rec digits index =
      if index < length then
        match value.[index] with
        | '0' .. '9' -> digits (index + 1)
        | _ -> index
      else index
    in
    if length < 2 || value.[0] <> 'v' then false
    else
      let after_major = digits 1 in
      if after_major = 1 then false
      else if after_major = length then true
      else
        let suffix prefix =
          String.starts_with ~prefix value
          &&
          let after_prefix = String.length prefix in
          after_prefix < length && digits after_prefix = length
        in
        suffix (String.sub value 0 after_major ^ "alpha")
        || suffix (String.sub value 0 after_major ^ "beta")

  let duplicates values =
    let sorted = List.sort String.compare values in
    let rec loop result = function
      | left :: (right :: _ as rest) when left = right ->
          loop (left :: result) rest
      | _ :: rest -> loop result rest
      | [] -> List.sort_uniq String.compare result
    in
    loop [] sorted

  let validate_path label path errors =
    if String.length path < 2 || path.[0] <> '.' then
      (label ^ " must be a non-empty JSONPath beginning with '.'") :: errors
    else errors

  let validate ~group ~kind ~plural ~singular ~list_kind ~short_names
      ~categories versions =
    let errors = [] in
    let errors =
      if valid_group group then errors
      else "group is not a DNS subdomain" :: errors
    in
    let errors =
      if String.length plural + 1 + String.length group <= 253 then errors
      else "plural.group CRD name exceeds 253 characters" :: errors
    in
    let errors =
      List.fold_left
        (fun errors (label, value) ->
          if valid_dns_label value then errors
          else (label ^ " is not a DNS label") :: errors)
        errors
        (("plural", plural) :: ("singular", singular)
         :: List.map (fun value -> ("short name", value)) short_names
        @ List.map (fun value -> ("category", value)) categories)
    in
    let errors =
      if valid_kind kind then errors
      else "kind must be an OCaml-style capitalized identifier" :: errors
    in
    let errors =
      if valid_kind list_kind then errors
      else "list kind must be an OCaml-style capitalized identifier" :: errors
    in
    let errors =
      if versions = [] then "at least one version is required" :: errors
      else errors
    in
    let storage_count =
      List.fold_left
        (fun count version -> if version.storage then count + 1 else count)
        0 versions
    in
    let errors =
      if storage_count = 1 then errors
      else "exactly one version must have storage=true" :: errors
    in
    let errors =
      List.fold_left
        (fun errors duplicate ->
          ("duplicate version name: " ^ duplicate) :: errors)
        errors
        (duplicates (List.map (fun version -> version.name) versions))
    in
    let errors =
      List.fold_left
        (fun errors version ->
          let prefix = "version " ^ version.name in
          let errors =
            if valid_version version.name then errors
            else (prefix ^ " has an invalid Kubernetes version name") :: errors
          in
          let errors =
            if Schema.is_object version.schema then errors
            else (prefix ^ " root schema must have type object") :: errors
          in
          let errors =
            if (not version.storage) || version.served then errors
            else
              (prefix ^ " is the storage version but is not served") :: errors
          in
          let errors =
            match Schema.validate version.schema with
            | Ok () -> errors
            | Error schema_errors ->
                List.rev_append
                  (List.map (fun error -> prefix ^ ": " ^ error) schema_errors)
                  errors
          in
          let errors =
            List.fold_left
              (fun errors duplicate ->
                (prefix ^ " has duplicate printer column " ^ duplicate)
                :: errors)
              errors
              (duplicates
                 (List.map
                    (fun (column : printer_column) -> column.name)
                    version.printer_columns))
          in
          let errors =
            List.fold_left
              (fun errors (column : printer_column) ->
                let errors =
                  if String.trim column.name = "" then
                    (prefix ^ " has an empty printer column name") :: errors
                  else errors
                in
                let errors =
                  validate_path
                    (prefix ^ " printer column " ^ column.name ^ " jsonPath")
                    column.json_path errors
                in
                match column.priority with
                | Some priority when priority < 0 ->
                    (prefix ^ " printer column priority must not be negative")
                    :: errors
                | _ -> errors)
              errors version.printer_columns
          in
          match version.scale with
          | None -> errors
          | Some scale ->
              let errors =
                validate_path
                  (prefix ^ " scale specReplicasPath")
                  scale.spec_replicas_path errors
              in
              let errors =
                validate_path
                  (prefix ^ " scale statusReplicasPath")
                  scale.status_replicas_path errors
              in
              Option.fold ~none:errors
                ~some:(fun path ->
                  validate_path
                    (prefix ^ " scale labelSelectorPath")
                    path errors)
                scale.label_selector_path)
        errors versions
    in
    match List.rev errors with
    | [] -> Ok ()
    | errors -> Error errors

  let make ?singular ?list_kind ?(short_names = []) ?(categories = []) ~group
      ~kind ~plural ~scope ~versions () =
    let singular =
      Option.value ~default:kind singular |> String.lowercase_ascii
    in
    let list_kind = Option.value ~default:(kind ^ "List") list_kind in
    match
      validate ~group ~kind ~plural ~singular ~list_kind ~short_names
        ~categories versions
    with
    | Error _ as error -> error
    | Ok () ->
        Ok
          {
            group;
            kind;
            plural;
            singular;
            list_kind;
            short_names;
            categories;
            scope;
            versions;
          }

  let make_exn ?singular ?list_kind ?short_names ?categories ~group ~kind
      ~plural ~scope ~versions () =
    match
      make ?singular ?list_kind ?short_names ?categories ~group ~kind ~plural
        ~scope ~versions ()
    with
    | Ok value -> value
    | Error errors -> invalid_arg (String.concat "; " errors)

  let optional name fn = function
    | None -> []
    | Some value -> [ (name, fn value) ]

  let string_list values = `List (List.map (fun value -> `String value) values)

  let scale_json (scale : scale) =
    `Assoc
      ([
         ("specReplicasPath", `String scale.spec_replicas_path);
         ("statusReplicasPath", `String scale.status_replicas_path);
       ]
      @ optional "labelSelectorPath"
          (fun value -> `String value)
          scale.label_selector_path)

  let printer_type = function
    | `Integer -> "integer"
    | `Number -> "number"
    | `String -> "string"
    | `Boolean -> "boolean"
    | `Date -> "date"

  let printer_column_json (column : printer_column) =
    `Assoc
      ([
         ("name", `String column.name);
         ("type", `String (printer_type column.type_));
         ("jsonPath", `String column.json_path);
       ]
      @ optional "format" (fun value -> `String value) column.format
      @ optional "description" (fun value -> `String value) column.description
      @ optional "priority" (fun value -> `Int value) column.priority)

  let version_json (version : version) =
    let subresources =
      (if version.status then [ ("status", `Assoc []) ] else [])
      @ optional "scale" scale_json version.scale
    in
    `Assoc
      ([
         ("name", `String version.name);
         ("served", `Bool version.served);
         ("storage", `Bool version.storage);
       ]
      @ (if subresources = [] then []
         else [ ("subresources", `Assoc subresources) ])
      @ [
          ( "schema",
            `Assoc [ ("openAPIV3Schema", Schema.to_json version.schema) ] );
        ]
      @
      if version.printer_columns = [] then []
      else
        [
          ( "additionalPrinterColumns",
            `List (List.map printer_column_json version.printer_columns) );
        ])

  let to_json (crd : t) =
    let names =
      [
        ("plural", `String crd.plural);
        ("singular", `String crd.singular);
        ("kind", `String crd.kind);
        ("listKind", `String crd.list_kind);
      ]
      @ (if crd.short_names = [] then []
         else [ ("shortNames", string_list crd.short_names) ])
      @
      if crd.categories = [] then []
      else [ ("categories", string_list crd.categories) ]
    in
    `Assoc
      [
        ("apiVersion", `String "apiextensions.k8s.io/v1");
        ("kind", `String "CustomResourceDefinition");
        ("metadata", `Assoc [ ("name", `String (crd.plural ^ "." ^ crd.group)) ]);
        ( "spec",
          `Assoc
            [
              ("group", `String crd.group);
              ( "scope",
                `String
                  (match crd.scope with
                  | Kube.Core.Namespaced -> "Namespaced"
                  | Kube.Core.Cluster -> "Cluster") );
              ("names", `Assoc names);
              ("versions", `List (List.map version_json crd.versions));
            ] );
      ]

  let to_yaml crd = Yaml.encode (to_json crd)
end

module Resource = struct
  module type Value = sig
    type t

    val schema : Schema.t
    val of_json : Yojson.Safe.t -> (t, string) result
    val to_json : t -> Yojson.Safe.t
  end

  module type Definition = sig
    module Spec : Value
    module Status : Value

    val group : string
    val version : string
    val kind : string
    val plural : string
    val singular : string
    val scope : Kube.Core.scope
    val short_names : string list
    val categories : string list
  end

  module Make (Definition : Definition) = struct
    type t = {
      api_version : string;
      kind : string;
      metadata : Kube.Core.object_meta;
      spec : Definition.Spec.t;
      status : Definition.Status.t option;
    }

    let api =
      {
        Kube.Core.group = Definition.group;
        version = Definition.version;
        kind = Definition.kind;
        plural = Definition.plural;
        scope = Definition.scope;
      }

    let metadata value = value.metadata

    let member name = function
      | `Assoc fields -> List.assoc_opt name fields
      | _ -> None

    let string = function
      | `String value -> Some value
      | _ -> None

    let required_string name json =
      match Option.bind (member name json) string with
      | Some value -> Ok value
      | None -> Error (name ^ " is required")

    let ( let* ) result fn =
      match result with
      | Ok value -> fn value
      | Error _ as error -> error

    let of_json json =
      let* api_version = required_string "apiVersion" json in
      let* kind = required_string "kind" json in
      let* metadata = Kube.Core.object_meta_of_json json in
      let* spec =
        match member "spec" json with
        | None -> Error "spec is required"
        | Some json -> Definition.Spec.of_json json
      in
      let* status =
        match member "status" json with
        | None | Some `Null -> Ok None
        | Some json -> Result.map Option.some (Definition.Status.of_json json)
      in
      Ok { api_version; kind; metadata; spec; status }

    let to_json value =
      `Assoc
        ([
           ("apiVersion", `String value.api_version);
           ("kind", `String value.kind);
           ("metadata", Kube.Core.object_meta_to_json value.metadata);
           ("spec", Definition.Spec.to_json value.spec);
         ]
        @
        match value.status with
        | None -> []
        | Some status -> [ ("status", Definition.Status.to_json status) ])

    let make ?(api_version = Kube.Core.api_version api)
        ?(kind = Definition.kind) ?status ~metadata ~spec () =
      { api_version; kind; metadata; spec; status }

    let with_status status value = { value with status }

    let status_merge_patch status =
      Kube.Client.Merge_patch
        (`Assoc [ ("status", Definition.Status.to_json status) ])

    let root_schema =
      Schema.object_ ~required:[ "spec" ]
        [
          ("spec", Definition.Spec.schema); ("status", Definition.Status.schema);
        ]

    let crd =
      Custom_resource_definition.make_exn ~group:Definition.group
        ~kind:Definition.kind ~plural:Definition.plural
        ~singular:Definition.singular ~short_names:Definition.short_names
        ~categories:Definition.categories ~scope:Definition.scope
        ~versions:
          [
            Custom_resource_definition.version ~name:Definition.version
              ~served:true ~storage:true ~status:true ~schema:root_schema ();
          ]
        ()
  end
end
