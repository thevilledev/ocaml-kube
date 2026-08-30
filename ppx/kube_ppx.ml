open Ppxlib
open Ast_builder.Default

let label_key =
  Attribute.declare "kube.key" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload (estring __))
    Fun.id

let label_schema =
  Attribute.declare "kube.schema" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload __)
    Fun.id

let label_description =
  Attribute.declare "kube.description" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload (estring __))
    Fun.id

let constructor_name =
  Attribute.declare "kube.name" Attribute.Context.constructor_declaration
    Ast_pattern.(single_expr_payload (estring __))
    Fun.id

let attributes =
  [
    Attribute.T label_key;
    Attribute.T label_schema;
    Attribute.T label_description;
    Attribute.T constructor_name;
  ]

let lident ~loc name = { loc; txt = Longident.Lident name }
let evar ~loc name = pexp_ident ~loc (lident ~loc name)
let pvar ~loc name = ppat_var ~loc { loc; txt = name }

let function_name type_name suffix =
  if type_name = "t" then suffix else type_name ^ "_" ^ suffix

let rec longident_to_string = function
  | Longident.Lident name -> name
  | Ldot (path, name) -> longident_to_string path ^ "." ^ name
  | Lapply _ -> "<applied-module>"

let render_type type_ = Format.asprintf "%a" Pprintast.core_type type_

let function_longident path suffix =
  match path with
  | Longident.Lident name -> Longident.Lident (function_name name suffix)
  | Ldot (prefix, "t") -> Ldot (prefix, suffix)
  | Ldot (prefix, name) -> Ldot (prefix, function_name name suffix)
  | Lapply _ -> path

let snake_to_lower_camel value =
  let output = Buffer.create (String.length value) in
  let uppercase = ref false in
  String.iter
    (fun character ->
      if character = '_' then uppercase := true
      else if !uppercase then (
        Buffer.add_char output (Char.uppercase_ascii character);
        uppercase := false)
      else Buffer.add_char output character)
    value;
  Buffer.contents output

let json_key label =
  match Attribute.get label_key label with
  | Some value -> value
  | None -> snake_to_lower_camel label.pld_name.txt

let constructor_tag constructor =
  Option.value ~default:constructor.pcd_name.txt
    (Attribute.get constructor_name constructor)

let option_type = function
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "option"; _ }, [ inner ]); _ } ->
      Some inner
  | _ -> None

let list_type = function
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "list"; _ }, [ inner ]); _ } ->
      Some inner
  | _ -> None

let string_map_value = function
  | {
      ptyp_desc =
        Ptyp_constr
          ( { txt = Lident "list"; _ },
            [
              {
                ptyp_desc =
                  Ptyp_tuple
                    [
                      {
                        ptyp_desc =
                          Ptyp_constr ({ txt = Lident "string"; _ }, []);
                        _;
                      };
                      value;
                    ];
                _;
              };
            ] );
      _;
    } -> Some value
  | _ -> None

let unsupported type_ description =
  Location.raise_errorf ~loc:type_.ptyp_loc
    "[@@deriving kube] does not support %s" description

let reject_builtin type_ path =
  match path with
  | Longident.Lident ("char" | "bytes" | "nativeint") ->
      unsupported type_ (longident_to_string path)
  | _ -> ()

let rec encode_type ~local_types type_ value =
  let loc = type_.ptyp_loc in
  match type_.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "string"; _ }, []) -> [%expr `String [%e value]]
  | Ptyp_constr ({ txt = Lident "int"; _ }, []) -> [%expr `Int [%e value]]
  | Ptyp_constr ({ txt = Lident "int32"; _ }, []) ->
      [%expr `Intlit (Int32.to_string [%e value])]
  | Ptyp_constr ({ txt = Lident "int64"; _ }, []) ->
      [%expr `Intlit (Int64.to_string [%e value])]
  | Ptyp_constr ({ txt = Lident "float"; _ }, []) -> [%expr `Float [%e value]]
  | Ptyp_constr ({ txt = Lident "bool"; _ }, []) -> [%expr `Bool [%e value]]
  | Ptyp_constr ({ txt = Lident "unit"; _ }, []) -> [%expr `Assoc []]
  | Ptyp_constr ({ txt = Ldot (Ldot (Lident "Yojson", "Safe"), "t"); _ }, []) ->
      value
  | Ptyp_constr ({ txt = Lident "option"; _ }, [ inner ]) ->
      let inner_value = "__kube_value" in
      let encoded = encode_type ~local_types inner (evar ~loc inner_value) in
      [%expr
        match [%e value] with
        | None -> `Null
        | Some [%p pvar ~loc inner_value] -> [%e encoded]]
  | Ptyp_constr ({ txt = Lident "list"; _ }, [ inner ]) -> (
      match string_map_value type_ with
      | Some map_value ->
          let encoded =
            encode_type ~local_types map_value (evar ~loc "__kube_value")
          in
          [%expr
            `Assoc
              (List.map
                 (fun (__kube_key, __kube_value) -> (__kube_key, [%e encoded]))
                 [%e value])]
      | None ->
          let encoded =
            encode_type ~local_types inner (evar ~loc "__kube_item")
          in
          [%expr `List (List.map (fun __kube_item -> [%e encoded]) [%e value])])
  | Ptyp_constr ({ txt = Lident "array"; _ }, [ inner ]) ->
      let encoded = encode_type ~local_types inner (evar ~loc "__kube_item") in
      [%expr
        `List
          (Array.to_list
             (Array.map (fun __kube_item -> [%e encoded]) [%e value]))]
  | Ptyp_tuple types ->
      let variables =
        List.mapi (fun index _ -> Printf.sprintf "__kube_tuple_%d" index) types
      in
      let pattern = ppat_tuple ~loc (List.map (pvar ~loc) variables) in
      let fields =
        List.map2
          (fun index (type_, variable) ->
            pexp_tuple ~loc
              [
                estring ~loc (Printf.sprintf "item%d" index);
                encode_type ~local_types type_ (evar ~loc variable);
              ])
          (List.init (List.length types) Fun.id)
          (List.combine types variables)
      in
      [%expr
        let [%p pattern] = [%e value] in
        `Assoc [%e elist ~loc fields]]
  | Ptyp_constr ({ txt = path; _ }, []) ->
      reject_builtin type_ path;
      pexp_apply ~loc
        (pexp_ident ~loc { loc; txt = function_longident path "to_json" })
        [ (Nolabel, value) ]
  | Ptyp_var _ -> unsupported type_ "type parameters"
  | Ptyp_arrow _ -> unsupported type_ "function values"
  | Ptyp_variant _ -> unsupported type_ "polymorphic variants"
  | Ptyp_object _ | Ptyp_class _ -> unsupported type_ "object types"
  | Ptyp_package _ -> unsupported type_ "first-class modules"
  | Ptyp_alias (inner, _) -> encode_type ~local_types inner value
  | Ptyp_poly (_, inner) -> encode_type ~local_types inner value
  | Ptyp_extension _ -> unsupported type_ "extension types"
  | Ptyp_open _ -> unsupported type_ "locally opened types"
  | Ptyp_any -> unsupported type_ "wildcard types"
  | Ptyp_constr ({ txt = path; _ }, _ :: _) ->
      unsupported type_ ("parameterized type " ^ longident_to_string path)

let error_prefix ~loc label result =
  [%expr
    match [%e result] with
    | Ok __kube_value -> Ok __kube_value
    | Error __kube_message ->
        Error ([%e estring ~loc label] ^ ": " ^ __kube_message)]

let rec decode_type ~local_types type_ json =
  let loc = type_.ptyp_loc in
  match type_.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "string"; _ }, []) ->
      [%expr
        match [%e json] with
        | `String value -> Ok value
        | _ -> Error "expected string"]
  | Ptyp_constr ({ txt = Lident "int"; _ }, []) ->
      [%expr
        match [%e json] with
        | `Int value -> Ok value
        | `Intlit value -> (
            match int_of_string_opt value with
            | Some value -> Ok value
            | None -> Error "integer is outside OCaml int range")
        | _ -> Error "expected integer"]
  | Ptyp_constr ({ txt = Lident "int32"; _ }, []) ->
      [%expr
        match [%e json] with
        | `Int value ->
            let converted = Int32.of_int value in
            if Int32.to_int converted = value then Ok converted
            else Error "integer is outside int32 range"
        | `Intlit value -> (
            try Ok (Int32.of_string value)
            with _ -> Error "integer is outside int32 range")
        | _ -> Error "expected int32"]
  | Ptyp_constr ({ txt = Lident "int64"; _ }, []) ->
      [%expr
        match [%e json] with
        | `Int value -> Ok (Int64.of_int value)
        | `Intlit value -> (
            try Ok (Int64.of_string value)
            with _ -> Error "integer is outside int64 range")
        | _ -> Error "expected int64"]
  | Ptyp_constr ({ txt = Lident "float"; _ }, []) ->
      [%expr
        match [%e json] with
        | `Float value -> Ok value
        | `Int value -> Ok (float_of_int value)
        | `Intlit value -> (
            try Ok (float_of_string value) with _ -> Error "invalid number")
        | _ -> Error "expected number"]
  | Ptyp_constr ({ txt = Lident "bool"; _ }, []) ->
      [%expr
        match [%e json] with
        | `Bool value -> Ok value
        | _ -> Error "expected boolean"]
  | Ptyp_constr ({ txt = Lident "unit"; _ }, []) ->
      [%expr
        match [%e json] with
        | `Assoc [] -> Ok ()
        | _ -> Error "expected empty object"]
  | Ptyp_constr ({ txt = Ldot (Ldot (Lident "Yojson", "Safe"), "t"); _ }, []) ->
      [%expr Ok [%e json]]
  | Ptyp_constr ({ txt = Lident "option"; _ }, [ inner ]) ->
      let decoded = decode_type ~local_types inner json in
      [%expr
        match [%e json] with
        | `Null -> Ok None
        | _ -> Result.map Option.some [%e decoded]]
  | Ptyp_constr ({ txt = Lident "list"; _ }, [ inner ]) -> (
      match string_map_value type_ with
      | Some map_value ->
          let decoded =
            decode_type ~local_types map_value (evar ~loc "__kube_json")
          in
          [%expr
            match [%e json] with
            | `Assoc fields ->
                let rec loop accumulator = function
                  | [] -> Ok (List.rev accumulator)
                  | (__kube_key, __kube_json) :: rest -> (
                      match [%e decoded] with
                      | Error message -> Error (__kube_key ^ ": " ^ message)
                      | Ok value ->
                          loop ((__kube_key, value) :: accumulator) rest)
                in
                loop [] fields
            | _ -> Error "expected object map"]
      | None ->
          let decoded =
            decode_type ~local_types inner (evar ~loc "__kube_json")
          in
          [%expr
            match [%e json] with
            | `List values ->
                let rec loop index accumulator = function
                  | [] -> Ok (List.rev accumulator)
                  | __kube_json :: rest -> (
                      match [%e decoded] with
                      | Error message ->
                          Error (Printf.sprintf "item %d: %s" index message)
                      | Ok value -> loop (index + 1) (value :: accumulator) rest
                      )
                in
                loop 0 [] values
            | _ -> Error "expected array"])
  | Ptyp_constr ({ txt = Lident "array"; _ }, [ inner ]) ->
      let list_type =
        { type_ with ptyp_desc = Ptyp_constr (lident ~loc "list", [ inner ]) }
      in
      let decoded = decode_type ~local_types list_type json in
      [%expr Result.map Array.of_list [%e decoded]]
  | Ptyp_tuple types ->
      let value_variables =
        List.mapi (fun index _ -> Printf.sprintf "__kube_value_%d" index) types
      in
      let finish =
        [%expr Ok [%e pexp_tuple ~loc (List.map (evar ~loc) value_variables)]]
      in
      let rec decode index types finish =
        match types with
        | [] -> finish
        | type_ :: rest ->
            let decoded =
              decode_type ~local_types type_ (evar ~loc "__kube_tuple_json")
              |> error_prefix ~loc (Printf.sprintf "tuple item %d" index)
            in
            let variable = pvar ~loc (List.nth value_variables index) in
            let rest = decode (index + 1) rest finish in
            [%expr
              match
                List.assoc_opt
                  [%e estring ~loc (Printf.sprintf "item%d" index)]
                  __kube_tuple_fields
              with
              | None ->
                  Error
                    [%e
                      estring ~loc
                        (Printf.sprintf "tuple item %d is required" index)]
              | Some __kube_tuple_json -> (
                  match [%e decoded] with
                  | Error _ as error -> error
                  | Ok [%p variable] -> [%e rest])]
      in
      let body = decode 0 types finish in
      [%expr
        match [%e json] with
        | `Assoc __kube_tuple_fields -> [%e body]
        | _ -> Error "expected tuple object"]
  | Ptyp_constr ({ txt = path; _ }, []) ->
      reject_builtin type_ path;
      pexp_apply ~loc
        (pexp_ident ~loc { loc; txt = function_longident path "of_json" })
        [ (Nolabel, json) ]
  | Ptyp_var _ -> unsupported type_ "type parameters"
  | Ptyp_arrow _ -> unsupported type_ "function values"
  | Ptyp_variant _ -> unsupported type_ "polymorphic variants"
  | Ptyp_object _ | Ptyp_class _ -> unsupported type_ "object types"
  | Ptyp_package _ -> unsupported type_ "first-class modules"
  | Ptyp_alias (inner, _) -> decode_type ~local_types inner json
  | Ptyp_poly (_, inner) -> decode_type ~local_types inner json
  | Ptyp_extension _ -> unsupported type_ "extension types"
  | Ptyp_open _ -> unsupported type_ "locally opened types"
  | Ptyp_any -> unsupported type_ "wildcard types"
  | Ptyp_constr ({ txt = path; _ }, _ :: _) ->
      unsupported type_ ("parameterized type " ^ longident_to_string path)

let rec schema_type ~local_types ?override type_ =
  match override with
  | Some schema -> schema
  | None -> (
      let loc = type_.ptyp_loc in
      match type_.ptyp_desc with
      | Ptyp_constr ({ txt = Lident "string"; _ }, []) ->
          [%expr Kube_crd.Schema.string ()]
      | Ptyp_constr ({ txt = Lident "int"; _ }, []) ->
          [%expr Kube_crd.Schema.integer ~format:`Int64 ()]
      | Ptyp_constr ({ txt = Lident "int32"; _ }, []) ->
          [%expr Kube_crd.Schema.integer ~format:`Int32 ()]
      | Ptyp_constr ({ txt = Lident "int64"; _ }, []) ->
          [%expr Kube_crd.Schema.integer ~format:`Int64 ()]
      | Ptyp_constr ({ txt = Lident "float"; _ }, []) ->
          [%expr Kube_crd.Schema.number ~format:`Double ()]
      | Ptyp_constr ({ txt = Lident "bool"; _ }, []) ->
          [%expr Kube_crd.Schema.boolean ()]
      | Ptyp_constr ({ txt = Lident "unit"; _ }, []) ->
          [%expr Kube_crd.Schema.object_ []]
      | Ptyp_constr ({ txt = Ldot (Ldot (Lident "Yojson", "Safe"), "t"); _ }, [])
        -> [%expr Kube_crd.Schema.preserve_unknown ()]
      | Ptyp_constr ({ txt = Lident "option"; _ }, [ inner ]) ->
          let inner = schema_type ~local_types inner in
          [%expr Kube_crd.Schema.nullable [%e inner]]
      | Ptyp_constr ({ txt = Lident "list"; _ }, [ inner ]) -> (
          match string_map_value type_ with
          | Some map_value ->
              let value = schema_type ~local_types map_value in
              [%expr Kube_crd.Schema.map [%e value]]
          | None ->
              let inner = schema_type ~local_types inner in
              [%expr Kube_crd.Schema.array [%e inner]])
      | Ptyp_constr ({ txt = Lident "array"; _ }, [ inner ]) ->
          let inner = schema_type ~local_types inner in
          [%expr Kube_crd.Schema.array [%e inner]]
      | Ptyp_tuple types ->
          let keys =
            List.mapi (fun index _ -> Printf.sprintf "item%d" index) types
          in
          let properties =
            List.map2
              (fun key type_ ->
                pexp_tuple ~loc
                  [ estring ~loc key; schema_type ~local_types type_ ])
              keys types
          in
          [%expr
            Kube_crd.Schema.object_
              ~required:[%e elist ~loc (List.map (estring ~loc) keys)]
              [%e elist ~loc properties]]
      | Ptyp_constr ({ txt = Lident name; _ }, [])
        when List.mem name local_types ->
          Location.raise_errorf ~loc
            "recursive or mutually recursive CRD schema reference to %s; add \
             [@kube.schema ...] at the recursion boundary"
            name
      | Ptyp_constr ({ txt = path; _ }, []) ->
          reject_builtin type_ path;
          pexp_ident ~loc { loc; txt = function_longident path "schema" }
      | Ptyp_var _ -> unsupported type_ "type parameters"
      | Ptyp_arrow _ -> unsupported type_ "function values"
      | Ptyp_variant _ -> unsupported type_ "polymorphic variants"
      | Ptyp_object _ | Ptyp_class _ -> unsupported type_ "object types"
      | Ptyp_package _ -> unsupported type_ "first-class modules"
      | Ptyp_alias (inner, _) -> schema_type ~local_types inner
      | Ptyp_poly (_, inner) -> schema_type ~local_types inner
      | Ptyp_extension _ -> unsupported type_ "extension types"
      | Ptyp_open _ -> unsupported type_ "locally opened types"
      | Ptyp_any -> unsupported type_ "wildcard types"
      | Ptyp_constr ({ txt = path; _ }, _ :: _) ->
          unsupported type_ ("parameterized type " ^ longident_to_string path))

let encode_record_fields ~local_types ~loc labels value_of =
  let entries =
    List.map
      (fun label ->
        let key = json_key label in
        let field = value_of label in
        match option_type label.pld_type with
        | None ->
            let encoded = encode_type ~local_types label.pld_type field in
            [%expr [ ([%e estring ~loc key], [%e encoded]) ]]
        | Some inner ->
            let encoded =
              encode_type ~local_types inner (evar ~loc "__kube_value")
            in
            [%expr
              match [%e field] with
              | None -> []
              | Some __kube_value -> [ ([%e estring ~loc key], [%e encoded]) ]])
      labels
  in
  [%expr `Assoc (List.concat [%e elist ~loc entries])]

let decode_record_fields ~local_types ~loc labels fields finish =
  let rec loop labels finish =
    match labels with
    | [] -> finish
    | label :: rest ->
        let key = json_key label in
        let variable = "__kube_" ^ label.pld_name.txt in
        let rest = loop rest finish in
        let lookup = [%expr List.assoc_opt [%e estring ~loc key] [%e fields]] in
        let decoded =
          match option_type label.pld_type with
          | Some inner ->
              let inner =
                decode_type ~local_types inner (evar ~loc "__kube_json")
                |> error_prefix ~loc key
              in
              [%expr
                match [%e lookup] with
                | None | Some `Null -> Ok None
                | Some __kube_json -> Result.map Option.some [%e inner]]
          | None ->
              let inner =
                decode_type ~local_types label.pld_type
                  (evar ~loc "__kube_json")
                |> error_prefix ~loc key
              in
              [%expr
                match [%e lookup] with
                | None -> Error [%e estring ~loc (key ^ " is required")]
                | Some __kube_json -> [%e inner]]
        in
        [%expr
          match [%e decoded] with
          | Error _ as error -> error
          | Ok [%p pvar ~loc variable] -> [%e rest]]
  in
  loop labels finish

let record_encoder ~local_types ~loc labels =
  encode_record_fields ~local_types ~loc labels (fun label ->
      pexp_field ~loc (evar ~loc "value")
        { loc; txt = Lident label.pld_name.txt })

let record_decoder ~local_types ~loc labels =
  let fields = evar ~loc "__kube_fields" in
  let record =
    pexp_record ~loc
      (List.map
         (fun label ->
           ( { loc; txt = Lident label.pld_name.txt },
             evar ~loc ("__kube_" ^ label.pld_name.txt) ))
         labels)
      None
  in
  let body =
    decode_record_fields ~local_types ~loc labels fields [%expr Ok [%e record]]
  in
  [%expr
    match __kube_json with
    | `Assoc __kube_fields -> [%e body]
    | _ -> Error "expected object"]

let record_schema ~local_types ~loc labels =
  let properties =
    List.map
      (fun label ->
        let type_ =
          Option.value ~default:label.pld_type (option_type label.pld_type)
        in
        let schema =
          schema_type ~local_types
            ?override:(Attribute.get label_schema label)
            type_
        in
        let schema =
          match Attribute.get label_description label with
          | None -> schema
          | Some description ->
              [%expr
                Kube_crd.Schema.describe [%e estring ~loc description]
                  [%e schema]]
        in
        pexp_tuple ~loc [ estring ~loc (json_key label); schema ])
      labels
  in
  let required =
    List.filter_map
      (fun label ->
        if option_type label.pld_type = None then
          Some (estring ~loc (json_key label))
        else None)
      labels
  in
  [%expr
    Kube_crd.Schema.object_ ~required:[%e elist ~loc required]
      [%e elist ~loc properties]]

let constructor_pattern ~loc constructor variables =
  let name = lident ~loc constructor.pcd_name.txt in
  match constructor.pcd_args with
  | Pcstr_tuple [] -> ppat_construct ~loc name None
  | Pcstr_tuple [ _ ] ->
      ppat_construct ~loc name (Some (pvar ~loc (List.hd variables)))
  | Pcstr_tuple _ ->
      ppat_construct ~loc name
        (Some (ppat_tuple ~loc (List.map (pvar ~loc) variables)))
  | Pcstr_record labels ->
      let fields =
        List.map2
          (fun label variable ->
            ({ loc; txt = Lident label.pld_name.txt }, pvar ~loc variable))
          labels variables
      in
      ppat_construct ~loc name (Some (ppat_record ~loc fields Closed))

let constructor_expression ~loc constructor payload =
  pexp_construct ~loc (lident ~loc constructor.pcd_name.txt) payload

let payload_keys types =
  match types with
  | [ _ ] -> [ "value" ]
  | _ -> List.mapi (fun index _ -> Printf.sprintf "item%d" index) types

let variant_encoder ~local_types ~loc constructors =
  let all_nullary =
    List.for_all
      (fun constructor -> constructor.pcd_args = Pcstr_tuple [])
      constructors
  in
  let cases =
    List.map
      (fun constructor ->
        if constructor.pcd_res <> None then
          Location.raise_errorf ~loc:constructor.pcd_loc
            "[@@deriving kube] does not support GADT constructors";
        let tag = constructor_tag constructor in
        let variables =
          match constructor.pcd_args with
          | Pcstr_tuple types ->
              List.mapi
                (fun index _ -> Printf.sprintf "__kube_arg_%d" index)
                types
          | Pcstr_record labels ->
              List.map (fun label -> "__kube_" ^ label.pld_name.txt) labels
        in
        let pattern = constructor_pattern ~loc constructor variables in
        let expression =
          if all_nullary then [%expr `String [%e estring ~loc tag]]
          else
            let payload =
              match constructor.pcd_args with
              | Pcstr_tuple [] -> None
              | Pcstr_tuple [ type_ ] ->
                  let encoded =
                    encode_type ~local_types type_
                      (evar ~loc (List.hd variables))
                  in
                  Some [%expr `Assoc [ ("value", [%e encoded]) ]]
              | Pcstr_tuple types ->
                  let fields =
                    List.map2
                      (fun key (type_, variable) ->
                        pexp_tuple ~loc
                          [
                            estring ~loc key;
                            encode_type ~local_types type_ (evar ~loc variable);
                          ])
                      (payload_keys types)
                      (List.combine types variables)
                  in
                  Some [%expr `Assoc [%e elist ~loc fields]]
              | Pcstr_record labels ->
                  Some
                    (encode_record_fields ~local_types ~loc labels (fun label ->
                         evar ~loc ("__kube_" ^ label.pld_name.txt)))
            in
            let fields =
              [
                pexp_tuple ~loc
                  [ estring ~loc "type"; [%expr `String [%e estring ~loc tag]] ];
              ]
              @
              match payload with
              | None -> []
              | Some payload ->
                  [ pexp_tuple ~loc [ estring ~loc "value"; payload ] ]
            in
            [%expr `Assoc [%e elist ~loc fields]]
        in
        case ~lhs:pattern ~guard:None ~rhs:expression)
      constructors
  in
  pexp_match ~loc (evar ~loc "value") cases

let variant_decoder ~local_types ~loc constructors =
  let all_nullary =
    List.for_all
      (fun constructor -> constructor.pcd_args = Pcstr_tuple [])
      constructors
  in
  if all_nullary then
    let cases =
      List.map
        (fun constructor ->
          let tag = constructor_tag constructor in
          case
            ~lhs:(ppat_constant ~loc (Pconst_string (tag, loc, None)))
            ~guard:None
            ~rhs:[%expr Ok [%e constructor_expression ~loc constructor None]])
        constructors
      @ [
          case ~lhs:(ppat_any ~loc) ~guard:None
            ~rhs:[%expr Error "unknown variant constructor"];
        ]
    in
    [%expr
      match __kube_json with
      | `String __kube_tag ->
          [%e pexp_match ~loc (evar ~loc "__kube_tag") cases]
      | _ -> Error "expected variant string"]
  else
    let cases =
      List.map
        (fun constructor ->
          let tag = constructor_tag constructor in
          let result =
            match constructor.pcd_args with
            | Pcstr_tuple [] ->
                [%expr Ok [%e constructor_expression ~loc constructor None]]
            | Pcstr_tuple types ->
                let payload_variables =
                  List.mapi
                    (fun index _ -> Printf.sprintf "__kube_payload_%d" index)
                    types
                in
                let payload_expression =
                  match payload_variables with
                  | [ variable ] -> evar ~loc variable
                  | variables ->
                      pexp_tuple ~loc (List.map (evar ~loc) variables)
                in
                let keys = payload_keys types in
                let finish =
                  [%expr
                    Ok
                      [%e
                        constructor_expression ~loc constructor
                          (Some payload_expression)]]
                in
                let rec decode keys types variables finish =
                  match (keys, types, variables) with
                  | [], [], [] -> finish
                  | key :: keys, type_ :: types, variable :: variables ->
                      let decoded =
                        decode_type ~local_types type_ (evar ~loc "__kube_item")
                        |> error_prefix ~loc ("value." ^ key)
                      in
                      let rest = decode keys types variables finish in
                      [%expr
                        match
                          List.assoc_opt [%e estring ~loc key]
                            __kube_payload_fields
                        with
                        | None ->
                            Error
                              [%e
                                estring ~loc ("value." ^ key ^ " is required")]
                        | Some __kube_item -> (
                            match [%e decoded] with
                            | Error _ as error -> error
                            | Ok [%p pvar ~loc variable] -> [%e rest])]
                  | _ -> assert false
                in
                let decoded = decode keys types payload_variables finish in
                [%expr
                  match List.assoc_opt "value" __kube_fields with
                  | Some (`Assoc __kube_payload_fields) -> [%e decoded]
                  | Some _ -> Error "value: expected object"
                  | None -> Error "value is required"]
            | Pcstr_record labels ->
                let record =
                  pexp_record ~loc
                    (List.map
                       (fun label ->
                         ( { loc; txt = Lident label.pld_name.txt },
                           evar ~loc ("__kube_" ^ label.pld_name.txt) ))
                       labels)
                    None
                in
                let constructed =
                  constructor_expression ~loc constructor (Some record)
                in
                let fields = evar ~loc "__kube_payload_fields" in
                let decoded =
                  decode_record_fields ~local_types ~loc labels fields
                    [%expr Ok [%e constructed]]
                in
                [%expr
                  match List.assoc_opt "value" __kube_fields with
                  | Some (`Assoc __kube_payload_fields) -> [%e decoded]
                  | Some _ -> Error "value: expected object"
                  | None -> Error "value is required"]
          in
          case
            ~lhs:(ppat_constant ~loc (Pconst_string (tag, loc, None)))
            ~guard:None ~rhs:result)
        constructors
      @ [
          case ~lhs:(ppat_any ~loc) ~guard:None
            ~rhs:[%expr Error "unknown variant constructor"];
        ]
    in
    [%expr
      match __kube_json with
      | `Assoc __kube_fields -> (
          match List.assoc_opt "type" __kube_fields with
          | Some (`String __kube_tag) ->
              [%e pexp_match ~loc (evar ~loc "__kube_tag") cases]
          | _ -> Error "type is required")
      | _ -> Error "expected tagged variant object"]

let payload_schema ~local_types ~loc constructor =
  match constructor.pcd_args with
  | Pcstr_tuple [] -> None
  | Pcstr_tuple types ->
      let keys = payload_keys types in
      let properties =
        List.map2
          (fun key type_ ->
            pexp_tuple ~loc [ estring ~loc key; schema_type ~local_types type_ ])
          keys types
      in
      let schema =
        [%expr
          Kube_crd.Schema.object_
            ~required:[%e elist ~loc (List.map (estring ~loc) keys)]
            [%e elist ~loc properties]]
      in
      Some ("tuple:" ^ String.concat "," (List.map render_type types), schema)
  | Pcstr_record labels ->
      let schema = record_schema ~local_types ~loc labels in
      Some ("inline:" ^ constructor.pcd_name.txt, schema)

let variant_schema ~local_types ~loc constructors =
  let tags = List.map constructor_tag constructors in
  let all_nullary =
    List.for_all
      (fun constructor -> constructor.pcd_args = Pcstr_tuple [])
      constructors
  in
  if all_nullary then
    [%expr
      Kube_crd.Schema.string
        ~enum:[%e elist ~loc (List.map (estring ~loc) tags)]
        ()]
  else
    let payloads =
      List.filter_map (payload_schema ~local_types ~loc) constructors
    in
    let value_schema =
      match payloads with
      | [] -> None
      | [ (_, schema) ] -> Some schema
      | (first_shape, first_schema) :: rest
        when List.for_all (fun (shape, _) -> shape = first_shape) rest ->
          Some first_schema
      | _ -> Some [%expr Kube_crd.Schema.preserve_unknown ()]
    in
    let properties =
      [
        pexp_tuple ~loc
          [
            estring ~loc "type";
            [%expr
              Kube_crd.Schema.string
                ~enum:[%e elist ~loc (List.map (estring ~loc) tags)]
                ()];
          ];
      ]
      @
      match value_schema with
      | None -> []
      | Some schema -> [ pexp_tuple ~loc [ estring ~loc "value"; schema ] ]
    in
    [%expr
      Kube_crd.Schema.object_ ~required:[ "type" ] [%e elist ~loc properties]]

let declaration_bodies ~include_schema ~local_types declaration =
  let loc = declaration.ptype_loc in
  if declaration.ptype_params <> [] then
    Location.raise_errorf ~loc
      "[@@deriving kube] does not yet support parameterized type declarations";
  let ensure_unique label entries =
    let seen = Hashtbl.create (List.length entries) in
    List.iter
      (fun (name, location) ->
        if String.trim name = "" then
          Location.raise_errorf ~loc:location
            "[@@deriving kube] %s must not be empty" label;
        if Hashtbl.mem seen name then
          Location.raise_errorf ~loc:location
            "[@@deriving kube] duplicate %s %S" label name;
        Hashtbl.add seen name ())
      entries
  in
  match (declaration.ptype_kind, declaration.ptype_manifest) with
  | Ptype_record labels, None ->
      ensure_unique "JSON field name"
        (List.map (fun label -> (json_key label, label.pld_loc)) labels);
      ( record_encoder ~local_types ~loc labels,
        record_decoder ~local_types ~loc labels,
        if include_schema then Some (record_schema ~local_types ~loc labels)
        else None )
  | Ptype_variant constructors, None ->
      ensure_unique "variant tag"
        (List.map
           (fun constructor ->
             (constructor_tag constructor, constructor.pcd_loc))
           constructors);
      ( variant_encoder ~local_types ~loc constructors,
        variant_decoder ~local_types ~loc constructors,
        if include_schema then
          Some (variant_schema ~local_types ~loc constructors)
        else None )
  | Ptype_abstract, Some manifest ->
      ( encode_type ~local_types manifest (evar ~loc "value"),
        decode_type ~local_types manifest (evar ~loc "__kube_json"),
        if include_schema then Some (schema_type ~local_types manifest)
        else None )
  | Ptype_open, _ ->
      Location.raise_errorf ~loc
        "[@@deriving kube] does not support extensible variant types"
  | Ptype_abstract, None ->
      Location.raise_errorf ~loc
        "[@@deriving kube] requires a concrete type definition"
  | (Ptype_record _ | Ptype_variant _), Some _ ->
      Location.raise_errorf ~loc
        "[@@deriving kube] does not support manifest record or variant types"

let rec type_references local_types type_ =
  match type_.ptyp_desc with
  | Ptyp_constr ({ txt = Lident name; _ }, arguments) ->
      List.mem name local_types
      || List.exists (type_references local_types) arguments
  | Ptyp_constr (_, arguments) | Ptyp_tuple arguments ->
      List.exists (type_references local_types) arguments
  | Ptyp_arrow (_, left, right) ->
      type_references local_types left || type_references local_types right
  | Ptyp_alias (inner, _) | Ptyp_poly (_, inner) | Ptyp_open (_, inner) ->
      type_references local_types inner
  | Ptyp_variant (rows, _, _) ->
      List.exists
        (fun row ->
          match row.prf_desc with
          | Rtag (_, _, arguments) ->
              List.exists (type_references local_types) arguments
          | Rinherit inner -> type_references local_types inner)
        rows
  | Ptyp_any
  | Ptyp_var _
  | Ptyp_object _
  | Ptyp_class _
  | Ptyp_package _
  | Ptyp_extension _ -> false

let declaration_references local_types declaration =
  match (declaration.ptype_kind, declaration.ptype_manifest) with
  | Ptype_record labels, _ ->
      List.exists
        (fun label -> type_references local_types label.pld_type)
        labels
  | Ptype_variant constructors, _ ->
      List.exists
        (fun constructor ->
          match constructor.pcd_args with
          | Pcstr_tuple types -> List.exists (type_references local_types) types
          | Pcstr_record labels ->
              List.exists
                (fun label -> type_references local_types label.pld_type)
                labels)
        constructors
  | Ptype_abstract, Some manifest -> type_references local_types manifest
  | Ptype_abstract, None | Ptype_open, _ -> false

let function_binding ~loc name parameter body =
  value_binding ~loc ~pat:(pvar ~loc name)
    ~expr:(pexp_fun ~loc Nolabel None (pvar ~loc parameter) body)

let generate_structure ~include_schema ~loc:_ ~path:_ (rec_flag, declarations) =
  (match rec_flag with
  | Nonrecursive ->
      List.iter
        (fun declaration ->
          if declaration.ptype_manifest = None then
            Location.raise_errorf ~loc:declaration.ptype_loc
              "[@@deriving kube] on type nonrec requires an alias")
        declarations
  | Recursive -> ());
  let local_types =
    List.map (fun declaration -> declaration.ptype_name.txt) declarations
  in
  let codec_rec_flag =
    if List.exists (declaration_references local_types) declarations then
      Recursive
    else Nonrecursive
  in
  let bodies =
    List.map (declaration_bodies ~include_schema ~local_types) declarations
  in
  let encoders =
    List.map2
      (fun declaration (encoder, _, _) ->
        function_binding ~loc:declaration.ptype_loc
          (function_name declaration.ptype_name.txt "to_json")
          "value" encoder)
      declarations bodies
  in
  let decoders =
    List.map2
      (fun declaration (_, decoder, _) ->
        function_binding ~loc:declaration.ptype_loc
          (function_name declaration.ptype_name.txt "of_json")
          "__kube_json" decoder)
      declarations bodies
  in
  let codecs =
    [
      pstr_value ~loc:(List.hd declarations).ptype_loc codec_rec_flag encoders;
      pstr_value ~loc:(List.hd declarations).ptype_loc codec_rec_flag decoders;
    ]
  in
  if not include_schema then codecs
  else
    let schemas =
      List.map2
        (fun declaration (_, _, schema) ->
          Ast_builder.Default.value_binding ~loc:declaration.ptype_loc
            ~pat:
              (pvar ~loc:declaration.ptype_loc
                 (function_name declaration.ptype_name.txt "schema"))
            ~expr:(Option.get schema))
        declarations bodies
    in
    codecs
    @ [ pstr_value ~loc:(List.hd declarations).ptype_loc Nonrecursive schemas ]

let type_of_declaration declaration =
  ptyp_constr ~loc:declaration.ptype_loc
    (lident ~loc:declaration.ptype_loc declaration.ptype_name.txt)
    (List.map fst declaration.ptype_params)

let generate_signature ~include_schema ~loc:_ ~path:_ (_rec_flag, declarations)
    =
  List.concat_map
    (fun declaration ->
      let loc = declaration.ptype_loc in
      if declaration.ptype_params <> [] then
        Location.raise_errorf ~loc
          "[@@deriving kube] does not yet support parameterized type \
           declarations";
      let type_ = type_of_declaration declaration in
      let json_type =
        ptyp_constr ~loc
          { loc; txt = Ldot (Ldot (Lident "Yojson", "Safe"), "t") }
          []
      in
      let result_type =
        ptyp_constr ~loc (lident ~loc "result")
          [ type_; ptyp_constr ~loc (lident ~loc "string") [] ]
      in
      let schema_type =
        ptyp_constr ~loc
          { loc; txt = Ldot (Ldot (Lident "Kube_crd", "Schema"), "t") }
          []
      in
      let codecs =
        [
          psig_value ~loc
            (value_description ~loc
               ~name:
                 {
                   loc;
                   txt = function_name declaration.ptype_name.txt "to_json";
                 }
               ~type_:(ptyp_arrow ~loc Nolabel type_ json_type)
               ~prim:[]);
          psig_value ~loc
            (value_description ~loc
               ~name:
                 {
                   loc;
                   txt = function_name declaration.ptype_name.txt "of_json";
                 }
               ~type_:(ptyp_arrow ~loc Nolabel json_type result_type)
               ~prim:[]);
        ]
      in
      if include_schema then
        codecs
        @ [
            psig_value ~loc
              (value_description ~loc
                 ~name:
                   {
                     loc;
                     txt = function_name declaration.ptype_name.txt "schema";
                   }
                 ~type_:schema_type ~prim:[]);
          ]
      else codecs)
    declarations

let () =
  Deriving.add "kube"
    ~str_type_decl:
      (Deriving.Generator.make_noarg
         (generate_structure ~include_schema:true)
         ~attributes)
    ~sig_type_decl:
      (Deriving.Generator.make_noarg
         (generate_signature ~include_schema:true)
         ~attributes)
  |> Deriving.ignore;
  Deriving.add "kube_json"
    ~str_type_decl:
      (Deriving.Generator.make_noarg
         (generate_structure ~include_schema:false)
         ~attributes)
    ~sig_type_decl:
      (Deriving.Generator.make_noarg
         (generate_signature ~include_schema:false)
         ~attributes)
  |> Deriving.ignore
