type scope = Namespaced | Cluster
type diagnostic = { path : string; reason : string }

type definition = {
  source : string;
  source_path : string;
  group : string;
  version : string;
  kind : string;
  plural : string;
  singular : string;
  scope : scope;
  short_names : string list;
  categories : string list;
  spec_schema : Yojson.Safe.t;
  status_schema : Yojson.Safe.t;
}

let ( let* ) result fn =
  match result with
  | Ok value -> fn value
  | Error _ as error -> error

let read_file path =
  try
    let channel = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr channel)
      (fun () ->
        let length = in_channel_length channel in
        if length > 16 * 1024 * 1024 then Error "CRD input exceeds 16 MiB"
        else Ok (really_input_string channel length))
  with Sys_error message -> Error message

let parse_document path contents =
  let trimmed = String.trim contents in
  if trimmed = "" then Error (path ^ ": empty CRD document")
  else if trimmed.[0] = '{' || trimmed.[0] = '[' then
    try Ok (Yojson.Safe.from_string contents)
    with Yojson.Json_error message -> Error (path ^ ": " ^ message)
  else
    match Yaml_lite.parse contents with
    | Ok value -> Ok value
    | Error message -> Error (path ^ ": " ^ message)

let assoc context = function
  | `Assoc fields -> Ok fields
  | _ -> Error (context ^ " must be an object")

let member fields name = List.assoc_opt name fields

let required_member context fields name =
  match member fields name with
  | Some value -> Ok value
  | None -> Error (context ^ "." ^ name ^ " is required")

let required_assoc context fields name =
  let* value = required_member context fields name in
  assoc (context ^ "." ^ name) value

let required_string context fields name =
  match member fields name with
  | Some (`String value) when String.trim value <> "" -> Ok value
  | Some _ -> Error (context ^ "." ^ name ^ " must be a non-empty string")
  | None -> Error (context ^ "." ^ name ^ " is required")

let optional_strings context fields name =
  match member fields name with
  | None -> Ok []
  | Some (`List values) ->
      let rec loop accumulator = function
        | [] -> Ok (List.rev accumulator)
        | `String value :: rest when String.trim value <> "" ->
            loop (value :: accumulator) rest
        | _ -> Error (context ^ "." ^ name ^ " must contain non-empty strings")
      in
      loop [] values
  | Some _ -> Error (context ^ "." ^ name ^ " must be a list")

let bool_member fields name =
  match member fields name with
  | Some (`Bool value) -> value
  | _ -> false

let list_member context fields name =
  match member fields name with
  | Some (`List values) -> Ok values
  | Some _ -> Error (context ^ "." ^ name ^ " must be a list")
  | None -> Error (context ^ "." ^ name ^ " is required")

let schema_property context schema name =
  let* fields = assoc context schema in
  let* properties = required_assoc context fields "properties" in
  required_member (context ^ ".properties") properties name

let required_names schema =
  match schema with
  | `Assoc fields -> (
      match member fields "required" with
      | Some (`List values) ->
          List.filter_map
            (function
              | `String value -> Some value
              | _ -> None)
            values
      | _ -> [])
  | _ -> []

let subresource_status_enabled version_context version_fields =
  match member version_fields "subresources" with
  | Some (`Assoc subresources) -> (
      match member subresources "status" with
      | Some (`Assoc _) -> Ok true
      | Some _ ->
          Error (version_context ^ ".subresources.status must be an object")
      | None -> Ok false)
  | Some _ -> Error (version_context ^ ".subresources must be an object")
  | None -> Ok false

let select_version ?version versions =
  let parsed =
    List.mapi
      (fun index value ->
        let context = Printf.sprintf "spec.versions[%d]" index in
        let* fields = assoc context value in
        let* name = required_string context fields "name" in
        Ok (context, name, fields))
      versions
  in
  let rec collect accumulator = function
    | [] -> Ok (List.rev accumulator)
    | Ok value :: rest -> collect (value :: accumulator) rest
    | (Error _ as error) :: _ -> error
  in
  let* parsed = collect [] parsed in
  let names = List.map (fun (_, name, _) -> name) parsed in
  let* () =
    if List.length names = List.length (List.sort_uniq String.compare names)
    then Ok ()
    else Error "spec.versions contains duplicate version names"
  in
  match version with
  | Some requested -> (
      match List.find_opt (fun (_, name, _) -> name = requested) parsed with
      | Some selected -> Ok selected
      | None -> Error ("CRD version not found: " ^ requested))
  | None -> (
      match
        List.filter (fun (_, _, fields) -> bool_member fields "storage") parsed
      with
      | [ selected ] -> Ok selected
      | [] -> Error "CRD has no storage version; pass --version explicitly"
      | _ ->
          Error "CRD has multiple storage versions; pass --version explicitly")

let load_crd ?version path =
  let* source = read_file path in
  let* document = parse_document path source in
  let* root = assoc "CRD" document in
  let* api_version = required_string "CRD" root "apiVersion" in
  let* () =
    if api_version = "apiextensions.k8s.io/v1" then Ok ()
    else Error ("unsupported CRD apiVersion: " ^ api_version)
  in
  let* kind_value = required_string "CRD" root "kind" in
  let* () =
    if kind_value = "CustomResourceDefinition" then Ok ()
    else Error ("expected kind CustomResourceDefinition, got " ^ kind_value)
  in
  let* metadata = required_assoc "CRD" root "metadata" in
  let* metadata_name = required_string "metadata" metadata "name" in
  let* spec = required_assoc "CRD" root "spec" in
  let* group = required_string "spec" spec "group" in
  let* scope_value = required_string "spec" spec "scope" in
  let* scope =
    match scope_value with
    | "Namespaced" -> Ok Namespaced
    | "Cluster" -> Ok Cluster
    | value -> Error ("spec.scope must be Namespaced or Cluster, got " ^ value)
  in
  let* names = required_assoc "spec" spec "names" in
  let* plural = required_string "spec.names" names "plural" in
  let* singular = required_string "spec.names" names "singular" in
  let* kind = required_string "spec.names" names "kind" in
  let* () =
    let expected = plural ^ "." ^ group in
    if metadata_name = expected then Ok ()
    else
      Error
        (Printf.sprintf "metadata.name must be %s for spec.names.plural/group"
           expected)
  in
  let* short_names = optional_strings "spec.names" names "shortNames" in
  let* categories = optional_strings "spec.names" names "categories" in
  let* versions = list_member "spec" spec "versions" in
  let* version_context, version, selected = select_version ?version versions in
  let* served =
    match member selected "served" with
    | Some (`Bool value) -> Ok value
    | Some _ -> Error (version_context ^ ".served must be a boolean")
    | None -> Ok true
  in
  let* () =
    if served then Ok ()
    else Error ("selected CRD version is not served: " ^ version)
  in
  let* status_enabled = subresource_status_enabled version_context selected in
  let* () =
    if status_enabled then Ok ()
    else
      Error
        (version_context
       ^ " must enable the status subresource for a generated operator")
  in
  let* schema = required_assoc version_context selected "schema" in
  let* root_schema =
    required_member (version_context ^ ".schema") schema "openAPIV3Schema"
  in
  let* spec_schema =
    schema_property
      (version_context ^ ".schema.openAPIV3Schema")
      root_schema "spec"
  in
  let* () =
    if List.mem "spec" (required_names root_schema) then Ok ()
    else Error (version_context ^ " schema must require .spec")
  in
  let* status_schema =
    schema_property
      (version_context ^ ".schema.openAPIV3Schema")
      root_schema "status"
  in
  Ok
    {
      source;
      source_path = path;
      group;
      version;
      kind;
      plural;
      singular;
      scope;
      short_names;
      categories;
      spec_schema;
      status_schema;
    }

let separated_case separator value =
  let output = Buffer.create (String.length value + 8) in
  let add_separator () =
    if Buffer.length output > 0 then
      let contents = Buffer.contents output in
      if contents.[String.length contents - 1] <> separator then
        Buffer.add_char output separator
  in
  String.iteri
    (fun index character ->
      match character with
      | 'A' .. 'Z' ->
          let previous_is_lower_or_digit =
            index > 0
            &&
            match value.[index - 1] with
            | 'a' .. 'z' | '0' .. '9' -> true
            | _ -> false
          in
          let acronym_boundary =
            index > 0
            && index + 1 < String.length value
            &&
            match (value.[index - 1], value.[index + 1]) with
            | 'A' .. 'Z', 'a' .. 'z' -> true
            | _ -> false
          in
          if previous_is_lower_or_digit || acronym_boundary then
            add_separator ();
          Buffer.add_char output (Char.lowercase_ascii character)
      | 'a' .. 'z' | '0' .. '9' -> Buffer.add_char output character
      | _ -> add_separator ())
    value;
  let result = Buffer.contents output in
  if result <> "" && result.[String.length result - 1] = separator then
    String.sub result 0 (String.length result - 1)
  else result

let kebab_case value = separated_case '-' value
let default_project_name definition = kebab_case definition.kind ^ "-operator"

let ocaml_reserved =
  [
    "and";
    "as";
    "assert";
    "begin";
    "class";
    "constraint";
    "do";
    "done";
    "downto";
    "else";
    "end";
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
    "let";
    "match";
    "method";
    "module";
    "mutable";
    "new";
    "nonrec";
    "object";
    "of";
    "open";
    "or";
    "private";
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

let snake_case value =
  let value = separated_case '_' value in
  let value = if value = "" then "value" else value in
  let value =
    match value.[0] with
    | '0' .. '9' -> "value_" ^ value
    | _ -> value
  in
  if List.mem value ocaml_reserved then value ^ "_" else value

let valid_project_name value =
  let length = String.length value in
  length > 0 && length <= 63
  && (match value.[0] with
    | 'a' .. 'z' | '0' .. '9' -> true
    | _ -> false)
  && (match value.[length - 1] with
    | 'a' .. 'z' | '0' .. '9' -> true
    | _ -> false)
  && String.for_all
       (function
         | 'a' .. 'z' | '0' .. '9' | '-' -> true
         | _ -> false)
       value

let constructor_name value =
  let output = Buffer.create (String.length value + 1) in
  let capitalize = ref true in
  String.iter
    (fun character ->
      match character with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' ->
          let character =
            if !capitalize then Char.uppercase_ascii character else character
          in
          Buffer.add_char output character;
          capitalize := false
      | _ -> capitalize := true)
    value;
  let result = Buffer.contents output in
  if result = "" then "Value"
  else
    match result.[0] with
    | '0' .. '9' -> "Value" ^ result
    | _ -> result

let json_assoc = function
  | `Assoc fields -> fields
  | _ -> []

let json_member schema name = List.assoc_opt name (json_assoc schema)

let json_bool schema name =
  match json_member schema name with
  | Some (`Bool value) -> value
  | _ -> false

let json_string schema name =
  match json_member schema name with
  | Some (`String value) -> Some value
  | _ -> None

let json_properties schema =
  match json_member schema "properties" with
  | Some (`Assoc fields) -> fields
  | _ -> []

let json_required schema =
  match json_member schema "required" with
  | Some (`List values) ->
      List.filter_map
        (function
          | `String value -> Some value
          | _ -> None)
        values
  | _ -> []

let nullable schema = json_bool schema "nullable"

type type_generator = {
  names : (string, int) Hashtbl.t;
  mutable declarations : (string * string) list;
  mutable diagnostics : diagnostic list;
}

let raw_fallback generator path reason =
  let diagnostic = { path; reason } in
  if not (List.mem diagnostic generator.diagnostics) then
    generator.diagnostics <- diagnostic :: generator.diagnostics;
  "Raw_json.t"

let fresh_type generator suggested =
  let base = snake_case suggested in
  let count = Option.value ~default:0 (Hashtbl.find_opt generator.names base) in
  Hashtbl.replace generator.names base (count + 1);
  if count = 0 then base else base ^ "_" ^ string_of_int (count + 1)

let unique_field used wire_name =
  let base = snake_case wire_name in
  let count = Option.value ~default:0 (Hashtbl.find_opt used base) in
  Hashtbl.replace used base (count + 1);
  if count = 0 then base else base ^ "_" ^ string_of_int (count + 1)

let string_enum schema =
  match json_member schema "enum" with
  | Some (`List values) ->
      let values =
        List.map
          (function
            | `String value -> Some value
            | _ -> None)
          values
      in
      if List.for_all Option.is_some values then
        Some (List.filter_map Fun.id values)
      else None
  | _ -> None

let rec type_expression generator ~path ~suggested schema =
  let base = base_type_expression generator ~path ~suggested schema in
  if nullable schema && base <> "Raw_json.t" then base ^ " option" else base

and base_type_expression generator ~path ~suggested schema =
  if json_bool schema "x-kubernetes-int-or-string" then "Int_or_string.t"
  else if json_bool schema "x-kubernetes-embedded-resource" then
    raw_fallback generator path
      "embedded Kubernetes resources require dynamic metadata preservation"
  else
    match string_enum schema with
    | Some values when values <> [] ->
        let constructors = List.map constructor_name values in
        if
          List.length constructors
          <> List.length (List.sort_uniq String.compare constructors)
        then
          raw_fallback generator path
            "string enum values collide after OCaml constructor normalization"
        else
          let name = fresh_type generator suggested in
          let body =
            List.map2
              (fun constructor wire_name ->
                constructor ^ " [@kube.name "
                ^ Printf.sprintf "%S" wire_name
                ^ "]")
              constructors values
            |> String.concat "\n  | "
            |> fun value -> "\n  | " ^ value
          in
          generator.declarations <- (name, body) :: generator.declarations;
          name
    | _ -> (
        match json_string schema "type" with
        | Some "string" -> "string"
        | Some "integer" -> (
            match json_string schema "format" with
            | Some "int64" -> "int64"
            | Some "int32" -> "int32"
            | _ -> "int")
        | Some "number" -> "float"
        | Some "boolean" -> "bool"
        | Some "array" -> (
            match json_member schema "items" with
            | Some items ->
                let item =
                  type_expression generator ~path:(path ^ "[]")
                    ~suggested:(suggested ^ "_item") items
                in
                "(" ^ item ^ ") list"
            | None ->
                raw_fallback generator path "array schema has no items schema")
        | Some "object" ->
            object_type_expression generator ~path ~suggested schema
        | Some unsupported ->
            raw_fallback generator path
              ("unsupported OpenAPI type " ^ unsupported)
        | None ->
            let reason =
              if json_member schema "$ref" <> None then
                "schema references are not supported in structural CRD input"
              else if json_member schema "oneOf" <> None then
                "oneOf unions do not yet have a lossless OCaml representation"
              else if json_member schema "anyOf" <> None then
                "anyOf unions do not yet have a lossless OCaml representation"
              else if json_member schema "allOf" <> None then
                "allOf composition does not yet have a lossless OCaml \
                 representation"
              else "schema has no supported structural type"
            in
            raw_fallback generator path reason)

and object_type_expression generator ~path ~suggested schema =
  if json_bool schema "x-kubernetes-preserve-unknown-fields" then
    raw_fallback generator path
      "x-kubernetes-preserve-unknown-fields requires lossless dynamic JSON"
  else
    let properties = json_properties schema in
    match (properties, json_member schema "additionalProperties") with
    | [], Some (`Assoc _ as values) ->
        let values =
          type_expression generator
            ~path:(path ^ ".additionalProperties")
            ~suggested:(suggested ^ "_value") values
        in
        "(string * " ^ values ^ ") list"
    | [], (None | Some (`Bool false)) -> "Empty_object.t"
    | [], Some _ ->
        raw_fallback generator path
          "untyped additionalProperties requires lossless dynamic JSON"
    | _ :: _, (None | Some (`Bool false)) ->
        let name = fresh_type generator suggested in
        let required = json_required schema in
        let used = Hashtbl.create (List.length properties) in
        let fields =
          List.map
            (fun (wire_name, field_schema) ->
              let field_name = unique_field used wire_name in
              let field_type =
                type_expression generator
                  ~path:(path ^ "." ^ wire_name)
                  ~suggested:(suggested ^ "_" ^ field_name)
                  field_schema
              in
              let field_type =
                if List.mem wire_name required || nullable field_schema then
                  field_type
                else field_type ^ " option"
              in
              Printf.sprintf "    %s : %s [@kube.key %S];" field_name field_type
                wire_name)
            properties
        in
        let body = "{\n" ^ String.concat "\n" fields ^ "\n  }" in
        generator.declarations <- (name, body) :: generator.declarations;
        name
    | _ :: _, Some _ ->
        raw_fallback generator path
          "objects combining properties and additionalProperties require \
           lossless dynamic JSON"

let render_value_module name ~path schema =
  let generator =
    { names = Hashtbl.create 32; declarations = []; diagnostics = [] }
  in
  let root_expression =
    type_expression generator ~path ~suggested:"model_value" schema
  in
  generator.declarations <- ("model", root_expression) :: generator.declarations;
  let declarations = List.rev generator.declarations in
  let type_declarations =
    declarations
    |> List.map (fun (name, body) ->
        "  type " ^ name ^ " = " ^ body ^ "\n  [@@deriving kube_json]")
    |> String.concat "\n\n"
  in
  let schema_json = Yojson.Safe.to_string schema in
  let source =
    Printf.sprintf
      {|module %s = struct
  module Raw_json = struct
    type t = Yojson.Safe.t

    let of_json value = Ok value
    let to_json value = value
    let schema = C.Schema.preserve_unknown ()
  end

  module Empty_object = struct
    type t = unit

    let of_json = function
      | `Assoc [] -> Ok ()
      | _ -> Error "expected an empty JSON object"

    let to_json () = `Assoc []
    let schema = C.Schema.object_ []
  end

  module Int_or_string = struct
    type t = Int of int | String of string

    let of_json = function
      | `Int value -> Ok (Int value)
      | `Intlit value -> (
          match int_of_string_opt value with
          | Some value -> Ok (Int value)
          | None -> Error "Int-or-String integer is outside the OCaml int range")
      | `String value -> Ok (String value)
      | _ -> Error "expected an integer or string"

    let to_json = function Int value -> `Int value | String value -> `String value
    let schema = C.Schema.int_or_string ()
  end

%s

  type t = model

  let of_json = model_of_json
  let to_json = model_to_json
  let schema = C.Schema.raw (Yojson.Safe.from_string %S)
end
|}
      name type_declarations schema_json
  in
  (source, List.rev generator.diagnostics)

let ocaml_string_list values =
  "[ " ^ String.concat "; " (List.map (Printf.sprintf "%S") values) ^ " ]"

let render_resource definition =
  let spec, spec_diagnostics =
    render_value_module "Spec" ~path:".spec" definition.spec_schema
  in
  let status, status_diagnostics =
    render_value_module "Status" ~path:".status" definition.status_schema
  in
  let diagnostics = spec_diagnostics @ status_diagnostics in
  let diagnostic_comment =
    match diagnostics with
    | [] -> []
    | values ->
        [
          "(* Dynamic JSON fallbacks selected by the scaffolder:";
          String.concat "\n"
            (List.map
               (fun diagnostic ->
                 Printf.sprintf "   - %s: %s" diagnostic.path diagnostic.reason)
               values);
          "   The exact Kubernetes schemas remain embedded below. *)";
          "";
        ]
  in
  let scope =
    match definition.scope with
    | Namespaced -> "K.Core.Namespaced"
    | Cluster -> "K.Core.Cluster"
  in
  let source =
    String.concat "\n"
      ([
         "(* Generated by ocaml-kube scaffold. Edit the model and reconcile \
          logic as your API evolves. *)";
       ]
      @ diagnostic_comment
      @ [
          "module K = Kube";
          "module C = Kube_crd";
          "";
          spec;
          status;
          "include";
          "  C.Resource.Make (struct";
          "    module Spec = Spec";
          "    module Status = Status";
          "";
          Printf.sprintf "    let group = %S" definition.group;
          Printf.sprintf "    let version = %S" definition.version;
          Printf.sprintf "    let kind = %S" definition.kind;
          Printf.sprintf "    let plural = %S" definition.plural;
          Printf.sprintf "    let singular = %S" definition.singular;
          "    let scope = " ^ scope;
          "    let short_names = " ^ ocaml_string_list definition.short_names;
          "    let categories = " ^ ocaml_string_list definition.categories;
          "  end)";
          "";
        ])
  in
  (source, diagnostics)

let diagnostics definition = snd (render_resource definition)

let render_operator definition project_name =
  Printf.sprintf
    {|module K = Kube
module Api = K.Client.For (Custom_resource)
module Finalizer = K.Controller.Finalizer (Custom_resource)
module Controller = K.Controller.Make (Custom_resource)

let finalizer = %S

let reconcile client (request : Controller.request) =
  match request.resource with
  | None -> Ok K.Controller.Done
  | Some resource ->
      Finalizer.run ~cancel:request.cancel client resource finalizer (function
        | Finalizer.Cleanup resource ->
            ignore resource;
            (* TODO: delete external or dependent state. *)
            Ok K.Controller.Done
        | Finalizer.Apply resource ->
            ignore resource;
            (* TODO: converge desired state and patch status with
               [Api.patch_status ~cancel:request.cancel]. *)
            Ok K.Controller.Done)

let () =
  let kubeconfig = ref None in
  let context = ref None in
  let namespace = ref None in
  let workers = ref 2 in
  let leader_elect = ref false in
  let leader_election_name = ref %S in
  let leader_election_namespace = ref None in
  let identity = ref None in
  let diagnostics_address = ref "127.0.0.1" in
  let diagnostics_port = ref 0 in
  let set target value = target := Some value in
  let arguments =
    [
      ("--kubeconfig", Arg.String (set kubeconfig), "PATH Kubeconfig path");
      ("--context", Arg.String (set context), "NAME Kubeconfig context");
      ( "--namespace",
        Arg.String (set namespace),
        "NAME Namespace to watch (all by default)" );
      ("--workers", Arg.Set_int workers, "N Concurrent reconciliations");
      ( "--leader-elect",
        Arg.Set leader_elect,
        "Enable coordination.k8s.io Lease leader election" );
      ( "--leader-election-name",
        Arg.Set_string leader_election_name,
        "NAME Leader-election Lease name" );
      ( "--leader-election-namespace",
        Arg.String (set leader_election_namespace),
        "NAME Leader-election Lease namespace" );
      ( "--identity",
        Arg.String (set identity),
        "ID Unique leader-election candidate identity" );
      ( "--diagnostics-address",
        Arg.Set_string diagnostics_address,
        "IP Diagnostics bind address" );
      ( "--diagnostics-port",
        Arg.Set_int diagnostics_port,
        "PORT Diagnostics port (0 disables the server)" );
    ]
  in
  Arg.parse arguments
    (fun value -> raise (Arg.Bad ("unexpected argument: " ^ value)))
    %S;
  if !diagnostics_port < 0 || !diagnostics_port > 65535 then
    raise (Arg.Bad "--diagnostics-port must be between 0 and 65535");
  let loaded =
    match !kubeconfig with
    | Some path -> K.Config.load_kubeconfig ?context:!context path
    | None -> K.Config.load_default ?context:!context ()
  in
  match loaded with
  | Error message ->
      Printf.eprintf "configuration error: %%s\n%%!" message;
      exit 2
  | Ok config -> (
      let cancel = K.Cancel.create () in
      let stop _ = K.Cancel.cancel cancel in
      Sys.set_signal Sys.sigint (Sys.Signal_handle stop);
      Sys.set_signal Sys.sigterm (Sys.Signal_handle stop);
      let client = K.Client.create ~logger:(K.Log.stderr ()) config in
      let result =
        Fun.protect
          ~finally:(fun () -> K.Client.close client)
          (fun () ->
            let process_identity =
              Option.value
                ~default:
                  (Printf.sprintf "%%s-%%d" (Unix.gethostname ()) (Unix.getpid ()))
                !identity
            in
            let manager = K.Manager.create ~cancel client in
            let health = K.Health.create () in
            let metrics = K.Metrics.create () in
            let controller =
              Controller.component ?namespace:!namespace ~workers:!workers
                ~metrics ~health ~reconcile ()
            in
            let run_controller_manager manager_cancel =
              let manager = K.Manager.create ~cancel:manager_cancel client in
              K.Manager.add manager controller;
              K.Manager.run manager
            in
            (if !diagnostics_port <> 0 then
               let diagnostics =
                 K.Diagnostics.create ~address:!diagnostics_address
                   ~port:!diagnostics_port ~health ~metrics ()
               in
               K.Manager.add manager (K.Diagnostics.component diagnostics));
            (if not !leader_elect then
               K.Manager.add manager controller
             else
               let identity = process_identity in
               let lease_namespace =
                 Option.value
                   ~default:(Option.value ~default:"default" config.namespace)
                   !leader_election_namespace
               in
               let election =
                 K.Leader_election.default ~namespace:lease_namespace
                   ~name:!leader_election_name ~identity
               in
               let leader =
                 K.Metrics.Gauge.create ~registry:metrics
                   ~name:"ocaml_kube_leader"
                   ~help:"Whether this replica is leader." ()
               in
               let transitions =
                 K.Metrics.Counter.create ~registry:metrics
                   ~name:"ocaml_kube_leader_transitions_total"
                   ~help:"Leadership transitions observed by this replica." ()
               in
               let leader_component =
                 K.Manager.component ~name:"leader-election"
                   (fun ~client ~cancel ->
                     let on_phase = function
                       | K.Leader_election.Waiting ->
                           K.Metrics.Gauge.set leader 0.;
                           Printf.printf "waiting for leadership as %%s\n%%!"
                             identity
                       | K.Leader_election.Leading ->
                           K.Metrics.Gauge.set leader 1.;
                           K.Metrics.Counter.inc transitions;
                           Printf.printf "acquired leadership as %%s\n%%!"
                             identity
                       | K.Leader_election.Stopped ->
                           K.Metrics.Gauge.set leader 0.;
                           Printf.printf "stopped leader election as %%s\n%%!"
                             identity
                     in
                     match
                       K.Leader_election.run ~cancel ~on_phase client election
                         run_controller_manager
                     with
                     | Ok K.Leader_election.Cancelled_before_leadership -> Ok ()
                     | Ok (K.Leader_election.Finished (Ok ())) -> Ok ()
                     | Ok (K.Leader_election.Finished (Error error)) ->
                         Error
                           (K.Client.Transport
                              (Format.asprintf "%%a" K.Manager.pp_error error))
                     | Error error ->
                         Error
                           (K.Client.Transport
                              (Format.asprintf "%%a" K.Leader_election.pp_error
                                 error)))
               in
               K.Manager.add manager leader_component);
            Result.map_error
              (Format.asprintf "%%a" K.Manager.pp_error)
              (K.Manager.run manager))
      in
      (match result with
      | Ok () -> ()
      | Error message ->
          Format.eprintf "controller failed: %%s@." message;
          exit 1))
|}
    (definition.plural ^ "." ^ definition.group ^ "/finalizer")
    project_name
    (project_name ^ " [OPTIONS]")

let render_dune_project project_name =
  Printf.sprintf
    {|(lang dune 3.15)
(name %s)

(package
 (name %s)
 (synopsis %S)
 (depends
  (ocaml (>= 5.1))
  kube
  yojson))
|}
    project_name project_name
    ("Kubernetes operator for " ^ project_name)

let render_opam project_name =
  Printf.sprintf
    {|opam-version: "2.0"
synopsis: %S
depends: [
  "dune" {>= "3.15"}
  "ocaml" {>= "5.1"}
  "kube"
  "yojson"
]
build: [
  ["dune" "build" "-p" name "-j" jobs]
]
|}
    ("Kubernetes operator for " ^ project_name)

let render_library_dune project_name dune_name =
  Printf.sprintf
    {|(library
 (name %s_resource)
 (public_name %s.resource)
 (wrapped false)
 (modules custom_resource)
 (libraries kube kube.crd yojson)
 (preprocess
  (pps kube.ppx)))
|}
    dune_name project_name

let render_binary_dune project_name =
  Printf.sprintf
    {|(executable
 (name main)
 (public_name %s)
 (modules main)
 (libraries kube kube.crd %s.resource))
|}
    project_name project_name

let render_rbac definition project_name =
  Printf.sprintf
    {|apiVersion: v1
kind: ServiceAccount
metadata:
  name: %s
  namespace: default
---
apiVersion: rbac.authorization.k8s.io/v1
kind: ClusterRole
metadata:
  name: %s
rules:
  - apiGroups: [%S]
    resources: [%S]
    verbs: [get, list, watch, create, update, patch, delete]
  - apiGroups: [%S]
    resources: [%S, %S]
    verbs: [get, update, patch]
  - apiGroups: ["coordination.k8s.io"]
    resources: ["leases"]
    verbs: [get, create, update]
---
apiVersion: rbac.authorization.k8s.io/v1
kind: ClusterRoleBinding
metadata:
  name: %s
roleRef:
  apiGroup: rbac.authorization.k8s.io
  kind: ClusterRole
  name: %s
subjects:
  - kind: ServiceAccount
    name: %s
    namespace: default
|}
    project_name project_name definition.group definition.plural
    definition.group
    (definition.plural ^ "/status")
    (definition.plural ^ "/finalizers")
    project_name project_name project_name

let render_deployment project_name =
  Printf.sprintf
    {|apiVersion: apps/v1
kind: Deployment
metadata:
  name: %s
  namespace: default
spec:
  replicas: 2
  strategy:
    type: RollingUpdate
    rollingUpdate:
      maxUnavailable: 1
      maxSurge: 1
  selector:
    matchLabels:
      app.kubernetes.io/name: %s
  template:
    metadata:
      labels:
        app.kubernetes.io/name: %s
    spec:
      serviceAccountName: %s
      terminationGracePeriodSeconds: 30
      securityContext:
        runAsNonRoot: true
        seccompProfile:
          type: RuntimeDefault
      containers:
        - name: operator
          image: ghcr.io/replace-me/%s:latest
          imagePullPolicy: IfNotPresent
          args:
            - --workers
            - "2"
            - --leader-elect
            - --leader-election-name
            - %s
            - --leader-election-namespace
            - default
            - --diagnostics-address
            - 0.0.0.0
            - --diagnostics-port
            - "8080"
          ports:
            - name: diagnostics
              containerPort: 8080
              protocol: TCP
          livenessProbe:
            httpGet:
              path: /healthz
              port: diagnostics
            initialDelaySeconds: 2
            periodSeconds: 10
            timeoutSeconds: 2
            failureThreshold: 3
          readinessProbe:
            httpGet:
              path: /readyz
              port: diagnostics
            initialDelaySeconds: 2
            periodSeconds: 5
            timeoutSeconds: 2
            failureThreshold: 3
          securityContext:
            allowPrivilegeEscalation: false
            readOnlyRootFilesystem: true
            capabilities:
              drop: ["ALL"]
          resources:
            requests:
              cpu: 50m
              memory: 64Mi
            limits:
              memory: 256Mi
|}
    project_name project_name project_name project_name project_name
    project_name

let rec sample_value schema =
  match string_enum schema with
  | Some (value :: _) -> `String value
  | _ -> (
      match json_string schema "type" with
      | Some "string" -> `String "replace-me"
      | Some "integer" -> `Int 1
      | Some "number" -> `Float 1.
      | Some "boolean" -> `Bool false
      | Some "array" -> `List []
      | Some "object" ->
          let required = json_required schema in
          `Assoc
            (json_properties schema
            |> List.filter (fun (name, _) -> List.mem name required)
            |> List.map (fun (name, value) -> (name, sample_value value)))
      | _ -> `Assoc [])

let render_sample definition =
  `Assoc
    [
      ( "apiVersion",
        `String
          (if definition.group = "" then definition.version
           else definition.group ^ "/" ^ definition.version) );
      ("kind", `String definition.kind);
      ( "metadata",
        `Assoc
          ([ ("name", `String (definition.singular ^ "-sample")) ]
          @
          match definition.scope with
          | Namespaced -> [ ("namespace", `String "default") ]
          | Cluster -> []) );
      ("spec", sample_value definition.spec_schema);
    ]
  |> Yojson.Safe.pretty_to_string
  |> fun value -> value ^ "\n"

let render_dockerfile project_name =
  Printf.sprintf
    {|FROM ocaml/opam:debian-12-ocaml-5.2 AS build
WORKDIR /src
COPY --chown=opam:opam . .
RUN opam install . --deps-only --yes
RUN opam exec -- dune build bin/main.exe

FROM debian:bookworm-slim
RUN apt-get update && apt-get install --yes --no-install-recommends ca-certificates \
    && rm -rf /var/lib/apt/lists/*
COPY --from=build /src/_build/default/bin/main.exe /usr/local/bin/%s
USER 65532:65532
ENTRYPOINT ["/usr/local/bin/%s"]
|}
    project_name project_name

let render_readme definition project_name =
  Printf.sprintf
    {|# %s

Generated by `ocaml-kube scaffold` from `%s` for `%s/%s`, Kind `%s`.

## Develop

```sh
opam install . --deps-only --with-test
opam exec -- dune build @all
opam exec -- dune exec %s -- --kubeconfig ~/.kube/config
```

Implement convergence and cleanup in `bin/main.ml`. The generated controller
already uses a keyed reflector cache, retries, cancellation, and a finalizer.

## Install

```sh
kubectl apply -f deploy/crd.yaml
kubectl apply -f deploy/rbac.yaml
kubectl apply -f deploy/deployment.yaml
kubectl apply -f deploy/sample.yaml
```

`deploy/sample.yaml` uses JSON syntax, which is valid YAML. Replace its sample
values before applying. Update the placeholder image in `deploy/deployment.yaml`
after publishing a container. The deployment runs two replicas with Lease leader
election and exposes `/healthz`, `/readyz`, and `/metrics` on port 8080.
|}
    project_name
    (Filename.basename definition.source_path)
    definition.group definition.version definition.kind project_name

let init_definition ?(version = "v1alpha1") ?plural ?singular
    ?(scope = Namespaced) ~group ~kind () =
  let singular = Option.value ~default:(kebab_case kind) singular in
  let plural = Option.value ~default:(singular ^ "s") plural in
  let module C = Kube_crd in
  let spec_schema =
    C.Schema.object_ ~required:[ "replicas"; "image" ]
      [
        ("replicas", C.Schema.integer ~minimum:0 ());
        ("image", C.Schema.string ~min_length:1 ());
      ]
  in
  let phase_schema =
    C.Schema.object_ ~required:[ "type" ]
      [
        ("type", C.Schema.string ~enum:[ "Pending"; "Ready"; "Failed" ] ());
        ( "value",
          C.Schema.object_ ~required:[ "value" ]
            [ ("value", C.Schema.string ()) ] );
      ]
  in
  let status_schema =
    C.Schema.object_
      ~required:[ "observedGeneration"; "phase" ]
      [
        ("observedGeneration", C.Schema.integer ~format:`Int64 ());
        ("phase", phase_schema);
        ("message", C.Schema.string ());
      ]
  in
  let root_schema =
    C.Schema.object_ ~required:[ "spec" ]
      [ ("spec", spec_schema); ("status", status_schema) ]
  in
  let kube_scope =
    match scope with
    | Namespaced -> Kube.Core.Namespaced
    | Cluster -> Kube.Core.Cluster
  in
  match
    C.Custom_resource_definition.make ~group ~kind ~plural ~singular
      ~scope:kube_scope
      ~versions:
        [
          C.Custom_resource_definition.version ~name:version ~served:true
            ~storage:true ~status:true ~schema:root_schema ();
        ]
      ()
  with
  | Error errors -> Error (String.concat "; " errors)
  | Ok crd ->
      Ok
        {
          source = C.Custom_resource_definition.to_yaml crd;
          source_path = "OCaml type definitions";
          group;
          version;
          kind;
          plural;
          singular;
          scope;
          short_names = [];
          categories = [];
          spec_schema = C.Schema.to_json spec_schema;
          status_schema = C.Schema.to_json status_schema;
        }

let render_init_resource definition =
  let scope =
    match definition.scope with
    | Namespaced -> "K.Core.Namespaced"
    | Cluster -> "K.Core.Cluster"
  in
  Printf.sprintf
    {|(* Generated by ocaml-kube init. These OCaml types are the source of truth
   for JSON codecs and deploy/crd.yaml. *)
module K = Kube
module C = Kube_crd

module Spec = struct
  type t = {
    replicas : int; [@kube.schema C.Schema.integer ~minimum:0 ()]
    image : string; [@kube.schema C.Schema.string ~min_length:1 ()]
  }
  [@@deriving kube]
end

module Phase = struct
  type t = Pending | Ready | Failed of string [@@deriving kube]
end

module Status = struct
  type t = {
    observed_generation : int64;
    phase : Phase.t;
    message : string option;
  }
  [@@deriving kube]
end

include
  C.Resource.Make (struct
    module Spec = Spec
    module Status = Status

    let group = %S
    let version = %S
    let kind = %S
    let plural = %S
    let singular = %S
    let scope = %s
    let short_names = []
    let categories = []
  end)
|}
    definition.group definition.version definition.kind definition.plural
    definition.singular scope

let render_init_tools_dune project_name =
  Printf.sprintf
    {|(executable
 (name generate_crd)
 (modules generate_crd)
 (libraries kube.crd %s.resource))

(rule
 (target crd.generated.yaml)
 (action
  (with-stdout-to
   %%{target}
   (run %%{exe:generate_crd.exe}))))

(rule
 (alias crd-check)
 (deps crd.generated.yaml ../deploy/crd.yaml)
 (action
  (diff ../deploy/crd.yaml crd.generated.yaml)))

(alias
 (name codegen-check)
 (deps
  (alias crd-check)))
|}
    project_name

let render_init_generator =
  {|let () =
  print_string
    (Kube_crd.Custom_resource_definition.to_yaml Custom_resource.crd)
|}

let render_init_readme definition project_name =
  Printf.sprintf
    {|# %s

Generated by `ocaml-kube init` as a type-first operator for `%s/%s`, Kind `%s`.
The `Spec`, `Status`, and `Phase` definitions in `lib/custom_resource.ml` are the
source of truth for JSON codecs and the Kubernetes structural schema.

## Develop

```sh
opam install . --deps-only --with-test
opam exec -- dune build @all @codegen-check
opam exec -- dune exec %s -- --kubeconfig ~/.kube/config
```

After editing the resource types, regenerate and verify the checked-in CRD:

```sh
opam exec -- dune exec tools/generate_crd.exe > deploy/crd.yaml
opam exec -- dune build @codegen-check
```

Implement convergence and cleanup in `bin/main.ml`. The generated controller
already uses a keyed reflector cache, retries, cancellation, finalizers, and
cache-synchronized readiness.

## Install

```sh
kubectl apply -f deploy/crd.yaml
kubectl apply -f deploy/rbac.yaml
kubectl apply -f deploy/deployment.yaml
kubectl apply -f deploy/sample.yaml
```

Update the placeholder image in `deploy/deployment.yaml` after publishing a
container. The deployment runs two replicas with Lease leader election and
exposes `/healthz`, `/readyz`, and `/metrics` on port 8080.
|}
    project_name definition.group definition.version definition.kind
    project_name

let render ?project_name definition =
  let project_name =
    Option.value ~default:(default_project_name definition) project_name
  in
  if not (valid_project_name project_name) then
    Error
      "project name must be 1-63 lowercase alphanumeric or hyphen characters \
       and must not begin or end with a hyphen"
  else
    let dune_name =
      String.map
        (function
          | '-' -> '_'
          | value -> value)
        project_name
    in
    let crd =
      if String.ends_with ~suffix:"\n" definition.source then definition.source
      else definition.source ^ "\n"
    in
    let resource, _diagnostics = render_resource definition in
    Ok
      [
        ("dune-project", render_dune_project project_name);
        (project_name ^ ".opam", render_opam project_name);
        ("lib/dune", render_library_dune project_name dune_name);
        ("lib/custom_resource.ml", resource);
        ("bin/dune", render_binary_dune project_name);
        ("bin/main.ml", render_operator definition project_name);
        ("deploy/crd.yaml", crd);
        ("deploy/rbac.yaml", render_rbac definition project_name);
        ("deploy/deployment.yaml", render_deployment project_name);
        ("deploy/sample.yaml", render_sample definition);
        ("Dockerfile", render_dockerfile project_name);
        ("README.md", render_readme definition project_name);
        (".gitignore", "_build/\n*.install\n");
      ]

let render_init ?project_name ?version ?plural ?singular ?scope ~group ~kind ()
    =
  let* definition =
    init_definition ?version ?plural ?singular ?scope ~group ~kind ()
  in
  let project_name =
    Option.value ~default:(default_project_name definition) project_name
  in
  let* files = render ~project_name definition in
  let files =
    List.map
      (function
        | "lib/custom_resource.ml", _ ->
            ("lib/custom_resource.ml", render_init_resource definition)
        | "README.md", _ ->
            ("README.md", render_init_readme definition project_name)
        | file -> file)
      files
  in
  Ok
    (files
    @ [
        ("tools/dune", render_init_tools_dune project_name);
        ("tools/generate_crd.ml", render_init_generator);
      ])

let path_exists path =
  try
    ignore (Unix.lstat path);
    true
  with Unix.Unix_error (Unix.ENOENT, _, _) -> false

let rec remove_tree path =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_DIR ->
      Sys.readdir path
      |> Array.iter (fun name -> remove_tree (Filename.concat path name));
      Unix.rmdir path
  | _ -> Unix.unlink path

let rec ensure_directory root relative =
  if relative = "." || relative = "" then ()
  else
    let parent = Filename.dirname relative in
    ensure_directory root parent;
    let path = Filename.concat root relative in
    if not (path_exists path) then Unix.mkdir path 0o755

let write_file root (relative, contents) =
  ensure_directory root (Filename.dirname relative);
  let path = Filename.concat root relative in
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let generate_files ~output files =
  if path_exists output then Error ("output path already exists: " ^ output)
  else
    let parent = Filename.dirname output in
    if not (path_exists parent) then
      Error ("output parent does not exist: " ^ parent)
    else
      let temporary =
        output ^ ".ocaml-kube-tmp-" ^ string_of_int (Unix.getpid ())
      in
      if path_exists temporary then
        Error ("temporary path already exists: " ^ temporary)
      else
        try
          Unix.mkdir temporary 0o755;
          Fun.protect
            ~finally:(fun () ->
              if path_exists temporary then remove_tree temporary)
            (fun () ->
              List.iter (write_file temporary) files;
              Unix.rename temporary output);
          Ok (List.map fst files)
        with
        | Unix.Unix_error (code, fn, argument) ->
            Error
              (Printf.sprintf "%s(%s): %s" fn argument (Unix.error_message code))
        | Sys_error message -> Error message

let generate ?project_name ~output definition =
  let* files = render ?project_name definition in
  generate_files ~output files

let generate_init ?project_name ?version ?plural ?singular ?scope ~output ~group
    ~kind () =
  let* files =
    render_init ?project_name ?version ?plural ?singular ?scope ~group ~kind ()
  in
  generate_files ~output files
