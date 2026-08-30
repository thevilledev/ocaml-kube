type t = {
  api_version : string;
  kind : string;
  metadata : Core.object_meta;
  value : Yojson.Safe.t;
}

let member name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let string = function
  | `String value -> Some value
  | _ -> None

let of_json value =
  match
    ( Option.bind (member "apiVersion" value) string,
      Option.bind (member "kind" value) string,
      Core.object_meta_of_json value )
  with
  | Some api_version, Some kind, Ok metadata ->
      Ok { api_version; kind; metadata; value }
  | None, _, _ -> Error "apiVersion is required"
  | _, None, _ -> Error "kind is required"
  | _, _, Error message -> Error message

let to_json resource = resource.value

let resource api =
  (module struct
    type nonrec t = t

    let api = api
    let metadata value = value.metadata
    let of_json = of_json
    let to_json = to_json
  end : Core.Resource
    with type t = t)

let get ?cancel ?resource_version ?resource_version_match client ~api ?namespace
    name =
  let module Resource = (val resource api) in
  let module Api = Client.For (Resource) in
  Api.get ?cancel ?resource_version ?resource_version_match client ?namespace
    name

let create ?cancel ?options client ~api ?namespace value =
  let module Resource = (val resource api) in
  let module Api = Client.For (Resource) in
  Api.create ?cancel ?options client ?namespace value

let delete ?cancel ?options client ~api ?namespace name =
  let module Resource = (val resource api) in
  let module Api = Client.For (Resource) in
  Api.delete ?cancel ?options client ?namespace name

let patch ?cancel ?options client ~api ?namespace name patch =
  let module Resource = (val resource api) in
  let module Api = Client.For (Resource) in
  Api.patch ?cancel ?options client ?namespace name patch

let list_all ?cancel ?namespace ?label_selector ?field_selector
    ?resource_version ?resource_version_match ?page_size client ~api =
  let module Resource = (val resource api) in
  let module Api = Client.For (Resource) in
  Api.list_all ?cancel ?namespace ?label_selector ?field_selector
    ?resource_version ?resource_version_match ?page_size client

let watch ?cancel ?namespace ?label_selector ?field_selector ?timeout_seconds
    ?allow_bookmarks ?resource_version_match ?send_initial_events
    ?max_event_bytes client ~api ~resource_version ~on_event =
  let module Resource = (val resource api) in
  let module Api = Client.For (Resource) in
  Api.watch ?cancel ?namespace ?label_selector ?field_selector ?timeout_seconds
    ?allow_bookmarks ?resource_version_match ?send_initial_events
    ?max_event_bytes client ~resource_version ~on_event
