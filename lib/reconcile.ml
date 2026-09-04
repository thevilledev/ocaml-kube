let ( let* ) result fn =
  match result with
  | Ok value -> fn value
  | Error _ as error -> error

let assoc context = function
  | `Assoc fields -> Ok fields
  | _ -> Error (context ^ " must be a JSON object")

let replace_field name value fields =
  (name, value) :: List.remove_assoc name fields

let bool_field name fields =
  match List.assoc_opt name fields with
  | Some (`Bool value) -> value
  | _ -> false

let string_field name fields =
  match List.assoc_opt name fields with
  | Some (`String value) -> Some value
  | _ -> None

module Make (Resource : Core.Resource) = struct
  module Api = Client.For (Resource)

  let type_meta desired =
    let* root = assoc "resource" (Resource.to_json desired) in
    let ensure field expected fields =
      match List.assoc_opt field fields with
      | None -> Ok ((field, `String expected) :: fields)
      | Some (`String actual) when actual = expected -> Ok fields
      | Some (`String actual) ->
          Error
            (Printf.sprintf "resource.%s is %S, expected %S" field actual
               expected)
      | Some _ -> Error ("resource." ^ field ^ " must be a string")
    in
    let* root = ensure "apiVersion" (Core.api_version Resource.api) root in
    let* root = ensure "kind" Resource.api.kind root in
    Ok root

  let ownership_scope owner_api owner child =
    match (owner_api.Core.scope, Resource.api.Core.scope) with
    | Core.Namespaced, Core.Cluster ->
        Error "a namespaced resource cannot own a cluster-scoped resource"
    | Core.Namespaced, Core.Namespaced -> (
        match (owner.Core.namespace, child.Core.namespace) with
        | None, _ -> Error "the namespaced owner has no namespace"
        | Some owner_namespace, None ->
            Error
              (Printf.sprintf
                 "the owned resource must declare namespace %s"
                 owner_namespace)
        | Some owner_namespace, Some child_namespace
          when owner_namespace <> child_namespace ->
            Error
              (Printf.sprintf
                 "cross-namespace ownership is invalid (%s cannot own %s)"
                 owner_namespace child_namespace)
        | _ -> Ok ())
    | Core.Cluster, (Core.Cluster | Core.Namespaced) -> Ok ()

  let same_owner (reference : Core.owner_reference) fields =
    string_field "uid" fields = Some reference.Core.uid

  let controller_owner fields = bool_field "controller" fields

  let set_controller_reference ~owner_api ~owner desired =
    let child = Resource.metadata desired in
    let* () = ownership_scope owner_api owner child in
    let* reference = Core.controller_owner_reference owner_api owner in
    let* root = type_meta desired in
    let* metadata =
      match List.assoc_opt "metadata" root with
      | None -> Ok []
      | Some value -> assoc "resource.metadata" value
    in
    let* references =
      match List.assoc_opt "ownerReferences" metadata with
      | None -> Ok []
      | Some (`List values) -> Ok values
      | Some _ -> Error "resource.metadata.ownerReferences must be an array"
    in
    let* decoded =
      let rec loop accumulator = function
        | [] -> Ok (List.rev accumulator)
        | value :: rest ->
            let* fields = assoc "resource.metadata.ownerReferences[]" value in
            loop ((value, fields) :: accumulator) rest
      in
      loop [] references
    in
    match
      List.find_opt
        (fun (_, fields) ->
          controller_owner fields && not (same_owner reference fields))
        decoded
    with
    | Some (_, fields) ->
        let description =
          match (string_field "kind" fields, string_field "name" fields) with
          | Some kind, Some name -> kind ^ "/" ^ name
          | _ -> "another resource"
        in
        Error ("resource already has controller owner " ^ description)
    | None ->
        let references =
          decoded
          |> List.filter_map (fun (json, fields) ->
              if same_owner reference fields then None else Some json)
          |> fun values -> values @ [ Core.owner_reference_to_json reference ]
        in
        let metadata =
          replace_field "ownerReferences" (`List references) metadata
        in
        let json = `Assoc (replace_field "metadata" (`Assoc metadata) root) in
        Resource.of_json json

  let validate_field_manager field_manager =
    let length = String.length field_manager in
    if String.trim field_manager = "" then
      Error (Client.Invalid_request "field_manager must not be empty")
    else if length > 128 then
      Error (Client.Invalid_request "field_manager must be at most 128 bytes")
    else Ok ()

  let apply ?cancel ?namespace ?(force = false) ?(dry_run = false)
      ?field_validation client ~field_manager desired =
    let* () = validate_field_manager field_manager in
    let metadata = Resource.metadata desired in
    let* () =
      if String.trim metadata.name = "" then
        Error (Client.Invalid_request "applied resource name must not be empty")
      else Ok ()
    in
    let* value =
      match type_meta desired with
      | Ok root -> Ok (`Assoc root)
      | Error message -> Error (Client.Invalid_request message)
    in
    let namespace =
      match namespace with
      | Some _ -> namespace
      | None -> metadata.namespace
    in
    let options =
      {
        Client.default_write_options with
        dry_run = (if dry_run then [ "All" ] else []);
        field_validation;
      }
    in
    Api.patch ?cancel ~options client ?namespace metadata.name
      (Client.Apply { value; field_manager; force })

  let apply_owned ?cancel ?namespace ?force ?dry_run ?field_validation client
      ~field_manager ~owner_api ~owner desired =
    match set_controller_reference ~owner_api ~owner desired with
    | Error message -> Error (Client.Invalid_request message)
    | Ok desired ->
        apply ?cancel ?namespace ?force ?dry_run ?field_validation client
          ~field_manager desired
end
