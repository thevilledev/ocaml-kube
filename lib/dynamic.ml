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

let with_default_type_meta api = function
  | `Assoc fields ->
      let fields =
        if List.mem_assoc "kind" fields then fields
        else ("kind", `String api.Core.kind) :: fields
      in
      let fields =
        if List.mem_assoc "apiVersion" fields then fields
        else ("apiVersion", `String (Core.api_version api)) :: fields
      in
      `Assoc fields
  | value -> value

let resource api =
  (module struct
    type nonrec t = t

    let api = api
    let metadata value = value.metadata
    let of_json value = of_json (with_default_type_meta api value)
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

let replace ?cancel ?options client ~api ?namespace name value =
  let module Resource = (val resource api) in
  let module Api = Client.For (Resource) in
  Api.replace ?cancel ?options client ?namespace name value

let delete ?cancel ?options client ~api ?namespace name =
  let module Resource = (val resource api) in
  let module Api = Client.For (Resource) in
  Api.delete ?cancel ?options client ?namespace name

let delete_collection ?cancel ?options ?namespace ?all_namespaces
    ?label_selector ?field_selector ?resource_version ?resource_version_match
    ?limit ?continue ?timeout_seconds client ~api =
  let module Resource = (val resource api) in
  let module Api = Client.For (Resource) in
  Api.delete_collection ?cancel ?options ?namespace ?all_namespaces
    ?label_selector ?field_selector ?resource_version ?resource_version_match
    ?limit ?continue ?timeout_seconds client

let patch ?cancel ?options client ~api ?namespace name patch =
  let module Resource = (val resource api) in
  let module Api = Client.For (Resource) in
  Api.patch ?cancel ?options client ?namespace name patch

let patch_status ?cancel ?options client ~api ?namespace name patch =
  let module Resource = (val resource api) in
  let module Api = Client.For (Resource) in
  Api.patch_status ?cancel ?options client ?namespace name patch

let replace_status ?cancel ?options client ~api ?namespace name value =
  let module Resource = (val resource api) in
  let module Api = Client.For (Resource) in
  Api.replace_status ?cancel ?options client ?namespace name value

let get_subresource ?cancel client ~api ?namespace name subresource =
  let module Resource = (val resource api) in
  let module Api = Client.For (Resource) in
  Api.get_subresource ?cancel client ?namespace name subresource

let create_subresource ?cancel ?options client ~api ?namespace ~name
    ~subresource value =
  let module Resource = (val resource api) in
  let module Api = Client.For (Resource) in
  Api.create_subresource ?cancel ?options client ?namespace ~name ~subresource
    value

let replace_subresource ?cancel ?options client ~api ?namespace ~name
    ~subresource value =
  let module Resource = (val resource api) in
  let module Api = Client.For (Resource) in
  Api.replace_subresource ?cancel ?options client ?namespace ~name ~subresource
    value

let patch_subresource ?cancel ?options client ~api ?namespace ~name ~subresource
    patch =
  let module Resource = (val resource api) in
  let module Api = Client.For (Resource) in
  Api.patch_subresource ?cancel ?options client ?namespace ~name ~subresource
    patch

let delete_subresource ?cancel ?options client ~api ?namespace name subresource
    =
  let module Resource = (val resource api) in
  let module Api = Client.For (Resource) in
  Api.delete_subresource ?cancel ?options client ?namespace name subresource

let stream_subresource ?cancel ?query ?max_error_body_bytes client ~api
    ?namespace name subresource ~on_chunk =
  let module Resource = (val resource api) in
  let module Api = Client.For (Resource) in
  Api.stream_subresource ?cancel ?query ?max_error_body_bytes client ?namespace
    name subresource ~on_chunk

let get_scale ?cancel client ~api ?namespace name =
  let module Resource = (val resource api) in
  let module Api = Client.For (Resource) in
  Api.get_scale ?cancel client ?namespace name

let replace_scale ?cancel ?options client ~api ?namespace name scale =
  let module Resource = (val resource api) in
  let module Api = Client.For (Resource) in
  Api.replace_scale ?cancel ?options client ?namespace name scale

let patch_scale ?cancel ?options client ~api ?namespace name patch =
  let module Resource = (val resource api) in
  let module Api = Client.For (Resource) in
  Api.patch_scale ?cancel ?options client ?namespace name patch

let logs ?cancel ?options ?max_body_bytes client ~api ?namespace name =
  let module Resource = (val resource api) in
  let module Api = Client.For (Resource) in
  Api.logs ?cancel ?options ?max_body_bytes client ?namespace name

let stream_logs ?cancel ?options ?max_error_body_bytes client ~api ?namespace
    name ~on_chunk =
  let module Resource = (val resource api) in
  let module Api = Client.For (Resource) in
  Api.stream_logs ?cancel ?options ?max_error_body_bytes client ?namespace name
    ~on_chunk

let list ?cancel ?namespace ?label_selector ?field_selector ?resource_version
    ?resource_version_match ?limit ?continue client ~api =
  let module Resource = (val resource api) in
  let module Api = Client.For (Resource) in
  Api.list ?cancel ?namespace ?label_selector ?field_selector ?resource_version
    ?resource_version_match ?limit ?continue client

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
