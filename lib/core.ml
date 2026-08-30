type scope = Namespaced | Cluster

module Resource_version = struct
  type t = string

  let of_string value = value
  let to_string value = value
end

module Object_key = struct
  type t = { namespace : string option; name : string }

  let make ?namespace name = { namespace; name }
  let compare = Stdlib.compare
  let equal a b = compare a b = 0

  let to_string = function
    | { namespace = None; name } -> name
    | { namespace = Some namespace; name } -> namespace ^ "/" ^ name
end

type owner_reference = {
  api_version : string;
  kind : string;
  name : string;
  uid : string;
  controller : bool;
  block_owner_deletion : bool;
}

type object_reference = {
  api_version : string;
  kind : string;
  namespace : string option;
  name : string;
  uid : string option;
  resource_version : Resource_version.t option;
  field_path : string option;
}

type object_meta = {
  name : string;
  namespace : string option;
  uid : string option;
  resource_version : Resource_version.t option;
  generation : int option;
  deletion_timestamp : string option;
  finalizers : string list;
  owner_references : owner_reference list;
  labels : (string * string) list;
  annotations : (string * string) list;
}

type api = {
  group : string;
  version : string;
  kind : string;
  plural : string;
  scope : scope;
}

module type Resource = sig
  type t

  val api : api
  val metadata : t -> object_meta
  val of_json : Yojson.Safe.t -> (t, string) result
  val to_json : t -> Yojson.Safe.t
end

let api_version api =
  if api.group = "" then api.version else api.group ^ "/" ^ api.version

let escape_segment value = Uri.pct_encode ~component:`Path value

let collection_path api ~namespace =
  let prefix =
    if api.group = "" then "/api/" ^ escape_segment api.version
    else "/apis/" ^ escape_segment api.group ^ "/" ^ escape_segment api.version
  in
  match (api.scope, namespace) with
  | Cluster, None -> Ok (prefix ^ "/" ^ escape_segment api.plural)
  | Cluster, Some _ -> Error (api.kind ^ " is cluster-scoped")
  | Namespaced, Some namespace ->
      Ok
        (prefix ^ "/namespaces/" ^ escape_segment namespace ^ "/"
       ^ escape_segment api.plural)
  | Namespaced, None -> Ok (prefix ^ "/" ^ escape_segment api.plural)

let object_path api ~namespace ~name =
  match collection_path api ~namespace with
  | Error _ as error -> error
  | Ok path -> Ok (path ^ "/" ^ escape_segment name)

let subresource_path api ~namespace ~name ~subresource =
  match object_path api ~namespace ~name with
  | Error _ as error -> error
  | Ok path -> Ok (path ^ "/" ^ escape_segment subresource)

let member name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let string_opt = function
  | `String value -> Some value
  | _ -> None

let int_opt = function
  | `Int value -> Some value
  | `Intlit value -> int_of_string_opt value
  | _ -> None

let string_map = function
  | `Assoc fields ->
      List.filter_map
        (fun (name, value) ->
          Option.map (fun value -> (name, value)) (string_opt value))
        fields
  | _ -> []

let owner_reference_of_json = function
  | `Assoc fields -> (
      let get name = List.assoc_opt name fields in
      match
        ( Option.bind (get "apiVersion") string_opt,
          Option.bind (get "kind") string_opt,
          Option.bind (get "name") string_opt,
          Option.bind (get "uid") string_opt )
      with
      | Some api_version, Some kind, Some name, Some uid ->
          let bool name =
            match get name with
            | Some (`Bool value) -> value
            | _ -> false
          in
          Ok
            {
              api_version;
              kind;
              name;
              uid;
              controller = bool "controller";
              block_owner_deletion = bool "blockOwnerDeletion";
            }
      | _ -> Error "ownerReference is missing apiVersion/kind/name/uid")
  | _ -> Error "ownerReference must be an object"

let object_meta_of_json json =
  match member "metadata" json with
  | Some (`Assoc fields) -> (
      let get name = List.assoc_opt name fields in
      match Option.bind (get "name") string_opt with
      | None -> Error "metadata.name is required"
      | Some name ->
          let strings name =
            match get name with
            | Some (`List values) -> List.filter_map string_opt values
            | _ -> []
          in
          let owner_references =
            match get "ownerReferences" with
            | Some (`List values) ->
                List.filter_map
                  (fun value ->
                    match owner_reference_of_json value with
                    | Ok value -> Some value
                    | Error _ -> None)
                  values
            | _ -> []
          in
          Ok
            {
              name;
              namespace = Option.bind (get "namespace") string_opt;
              uid = Option.bind (get "uid") string_opt;
              resource_version =
                Option.map Resource_version.of_string
                  (Option.bind (get "resourceVersion") string_opt);
              generation = Option.bind (get "generation") int_opt;
              deletion_timestamp =
                Option.bind (get "deletionTimestamp") string_opt;
              finalizers = strings "finalizers";
              owner_references;
              labels = Option.fold ~none:[] ~some:string_map (get "labels");
              annotations =
                Option.fold ~none:[] ~some:string_map (get "annotations");
            })
  | _ -> Error "metadata object is required"

let owner_reference_to_json (reference : owner_reference) =
  `Assoc
    [
      ("apiVersion", `String reference.api_version);
      ("kind", `String reference.kind);
      ("name", `String reference.name);
      ("uid", `String reference.uid);
      ("controller", `Bool reference.controller);
      ("blockOwnerDeletion", `Bool reference.block_owner_deletion);
    ]

let object_reference ?field_path api (metadata : object_meta) =
  {
    api_version = api_version api;
    kind = api.kind;
    namespace = metadata.namespace;
    name = metadata.name;
    uid = metadata.uid;
    resource_version = metadata.resource_version;
    field_path;
  }

let object_reference_to_json (reference : object_reference) =
  let optional name fn = function
    | None -> []
    | Some value -> [ (name, fn value) ]
  in
  `Assoc
    ([
       ("apiVersion", `String reference.api_version);
       ("kind", `String reference.kind);
       ("name", `String reference.name);
     ]
    @ optional "namespace" (fun value -> `String value) reference.namespace
    @ optional "uid" (fun value -> `String value) reference.uid
    @ optional "resourceVersion"
        (fun value -> `String (Resource_version.to_string value))
        reference.resource_version
    @ optional "fieldPath" (fun value -> `String value) reference.field_path)

let make_owner_reference ?(controller = false) ?(block_owner_deletion = false)
    api (metadata : object_meta) =
  match metadata.uid with
  | None -> Error (api.kind ^ " " ^ metadata.name ^ " has no UID")
  | Some uid ->
      Ok
        {
          api_version = api_version api;
          kind = api.kind;
          name = metadata.name;
          uid;
          controller;
          block_owner_deletion;
        }

let controller_owner_reference api metadata =
  make_owner_reference ~controller:true ~block_owner_deletion:true api metadata

let object_meta_to_json metadata =
  let optional name fn = function
    | None -> []
    | Some value -> [ (name, fn value) ]
  in
  let string_map values =
    `Assoc (List.map (fun (name, value) -> (name, `String value)) values)
  in
  `Assoc
    ([ ("name", `String metadata.name) ]
    @ optional "namespace" (fun value -> `String value) metadata.namespace
    @ optional "uid" (fun value -> `String value) metadata.uid
    @ optional "resourceVersion"
        (fun value -> `String (Resource_version.to_string value))
        metadata.resource_version
    @ optional "generation" (fun value -> `Int value) metadata.generation
    @ optional "deletionTimestamp"
        (fun value -> `String value)
        metadata.deletion_timestamp
    @ (if metadata.finalizers = [] then []
       else
         [
           ( "finalizers",
             `List (List.map (fun value -> `String value) metadata.finalizers)
           );
         ])
    @ (if metadata.owner_references = [] then []
       else
         [
           ( "ownerReferences",
             `List (List.map owner_reference_to_json metadata.owner_references)
           );
         ])
    @ (if metadata.labels = [] then []
       else [ ("labels", string_map metadata.labels) ])
    @
    if metadata.annotations = [] then []
    else [ ("annotations", string_map metadata.annotations) ])

let key_of_meta metadata =
  { Object_key.namespace = metadata.namespace; name = metadata.name }
