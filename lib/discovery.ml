type resource = {
  name : string;
  singular_name : string;
  namespaced : bool;
  kind : string;
  verbs : string list;
  short_names : string list;
  categories : string list;
}

type resource_list = { group_version : string; resources : resource list }
type group_version = { group_version : string; version : string }

type group = {
  name : string;
  versions : group_version list;
  preferred_version : group_version option;
}

let ( let* ) result fn =
  match result with
  | Ok value -> fn value
  | Error _ as error -> error

let member name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let string = function
  | `String value -> Some value
  | _ -> None

let bool = function
  | `Bool value -> Some value
  | _ -> None

let strings = function
  | `List values -> List.filter_map string values
  | _ -> []

let decode response =
  try Ok (Yojson.Safe.from_string response.Http.body)
  with Yojson.Json_error message -> Error (Client.Decode message)

let server_version ?cancel client =
  let* response = Client.raw ?cancel client `GET "/version" in
  decode response

let core_versions ?cancel client =
  let* response = Client.raw ?cancel client `GET "/api" in
  let* json = decode response in
  match member "versions" json with
  | Some value -> Ok (strings value)
  | None -> Error (Client.Decode "core discovery response has no versions")

let group_version_of_json json =
  match
    ( Option.bind (member "groupVersion" json) string,
      Option.bind (member "version" json) string )
  with
  | Some group_version, Some version -> Some { group_version; version }
  | _ -> None

let group_of_json json =
  match Option.bind (member "name" json) string with
  | None -> None
  | Some name ->
      let versions =
        match member "versions" json with
        | Some (`List values) -> List.filter_map group_version_of_json values
        | _ -> []
      in
      let preferred_version =
        Option.bind (member "preferredVersion" json) group_version_of_json
      in
      Some { name; versions; preferred_version }

let groups ?cancel client =
  let* response = Client.raw ?cancel client `GET "/apis" in
  let* json = decode response in
  match member "groups" json with
  | Some (`List values) -> Ok (List.filter_map group_of_json values)
  | _ -> Error (Client.Decode "API group discovery response has no groups")

let resource_of_json json =
  match
    ( Option.bind (member "name" json) string,
      Option.bind (member "namespaced" json) bool,
      Option.bind (member "kind" json) string )
  with
  | Some name, Some namespaced, Some kind ->
      Some
        {
          name;
          singular_name =
            Option.bind (member "singularName" json) string
            |> Option.value ~default:"";
          namespaced;
          kind;
          verbs = Option.fold ~none:[] ~some:strings (member "verbs" json);
          short_names =
            Option.fold ~none:[] ~some:strings (member "shortNames" json);
          categories =
            Option.fold ~none:[] ~some:strings (member "categories" json);
        }
  | _ -> None

let resources ?cancel client ~group ~version =
  let path =
    if group = "" then "/api/" ^ Uri.pct_encode ~component:`Path version
    else
      "/apis/"
      ^ Uri.pct_encode ~component:`Path group
      ^ "/"
      ^ Uri.pct_encode ~component:`Path version
  in
  let* response = Client.raw ?cancel client `GET path in
  let* json = decode response in
  let* group_version =
    match Option.bind (member "groupVersion" json) string with
    | Some value -> Ok value
    | None ->
        Error (Client.Decode "resource discovery response has no groupVersion")
  in
  match member "resources" json with
  | Some (`List values) ->
      Ok { group_version; resources = List.filter_map resource_of_json values }
  | _ -> Error (Client.Decode "resource discovery response has no resources")
