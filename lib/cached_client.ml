module Make (Resource : Core.Resource) = struct
  module Cache = Reflector.Make (Resource)
  module Live = Client.For (Resource)

  type t = { client : Client.t; cache : Cache.t }

  let make ~client ~cache = { client; cache }
  let client reader = reader.client
  let cache reader = reader.cache
  let await_ready ~cancel reader = Cache.await_ready ~cancel reader.cache

  let with_ready reader fn =
    if Cache.is_ready reader.cache then fn ()
    else Error (Client.Transport "cache is not synchronized")

  let invalid_scope message = Error (Client.Invalid_request message)

  let not_found key =
    Error
      (Client.Api
         {
           code = 404;
           reason = Some "NotFound";
           message =
             Printf.sprintf "%s %s is absent from the synchronized cache"
               Resource.api.kind
               (Core.Object_key.to_string key);
           retry_after_seconds = None;
           body = None;
         })

  let validate_key key =
    match (Resource.api.scope, key.Core.Object_key.namespace) with
    | Core.Cluster, None | Core.Namespaced, Some _ -> Ok ()
    | Core.Cluster, Some _ ->
        invalid_scope
          (Resource.api.kind ^ " is cluster-scoped and cannot have a namespace")
    | Core.Namespaced, None ->
        invalid_scope (Resource.api.kind ^ " requires a namespace")

  let get_by_key reader key =
    with_ready reader (fun () ->
        match validate_key key with
        | Error _ as error -> error
        | Ok () -> (
            match Cache.get reader.cache key with
            | Some value -> Ok value
            | None -> not_found key))

  let get reader ?namespace name =
    let key =
      match Resource.api.scope with
      | Core.Cluster -> Core.Object_key.make ?namespace name
      | Core.Namespaced ->
          let namespace =
            match (namespace, (Client.config reader.client).namespace) with
            | Some namespace, _ -> namespace
            | None, Some namespace -> namespace
            | None, None -> "default"
          in
          Core.Object_key.make ~namespace name
    in
    get_by_key reader key

  let list ?namespace reader =
    with_ready reader (fun () ->
        match (Resource.api.scope, namespace) with
        | Core.Cluster, Some _ ->
            invalid_scope
              (Resource.api.kind
             ^ " is cluster-scoped and cannot be listed in a namespace")
        | _ ->
            Cache.items reader.cache
            |> List.filter (fun value ->
                match namespace with
                | None -> true
                | Some namespace ->
                    (Resource.metadata value).namespace = Some namespace)
            |> List.sort (fun left right ->
                Core.Object_key.compare
                  (Core.key_of_meta (Resource.metadata left))
                  (Core.key_of_meta (Resource.metadata right)))
            |> fun values -> Ok values)

  let by_index reader ~name value =
    with_ready reader (fun () ->
        match Cache.by_index reader.cache ~name value with
        | Ok values -> Ok values
        | Error message -> Error (Client.Invalid_request message))

  let fresh_get ?cancel ?resource_version ?resource_version_match reader
      ?namespace name =
    Live.get ?cancel ?resource_version ?resource_version_match reader.client
      ?namespace name

  let fresh_list_all ?cancel ?namespace ?label_selector ?field_selector
      ?resource_version ?resource_version_match ?page_size reader =
    Live.list_all ?cancel ?namespace ?label_selector ?field_selector
      ?resource_version ?resource_version_match ?page_size reader.client

  let create ?cancel ?options reader ?namespace value =
    Live.create ?cancel ?options reader.client ?namespace value

  let replace ?cancel ?options reader ?namespace name value =
    Live.replace ?cancel ?options reader.client ?namespace name value

  let delete ?cancel ?options reader ?namespace name =
    Live.delete ?cancel ?options reader.client ?namespace name

  let delete_collection ?cancel ?options ?namespace ?all_namespaces
      ?label_selector ?field_selector ?resource_version ?resource_version_match
      ?limit ?continue ?timeout_seconds reader =
    Live.delete_collection ?cancel ?options ?namespace ?all_namespaces
      ?label_selector ?field_selector ?resource_version ?resource_version_match
      ?limit ?continue ?timeout_seconds reader.client

  let patch ?cancel ?options reader ?namespace name value =
    Live.patch ?cancel ?options reader.client ?namespace name value

  let patch_status ?cancel ?options reader ?namespace name value =
    Live.patch_status ?cancel ?options reader.client ?namespace name value

  let replace_status ?cancel ?options reader ?namespace name value =
    Live.replace_status ?cancel ?options reader.client ?namespace name value

  let get_subresource ?cancel reader ?namespace name subresource =
    Live.get_subresource ?cancel reader.client ?namespace name subresource

  let create_subresource ?cancel ?options reader ?namespace ~name ~subresource
      value =
    Live.create_subresource ?cancel ?options reader.client ?namespace ~name
      ~subresource value

  let replace_subresource ?cancel ?options reader ?namespace ~name ~subresource
      value =
    Live.replace_subresource ?cancel ?options reader.client ?namespace ~name
      ~subresource value

  let patch_subresource ?cancel ?options reader ?namespace ~name ~subresource
      value =
    Live.patch_subresource ?cancel ?options reader.client ?namespace ~name
      ~subresource value

  let delete_subresource ?cancel ?options reader ?namespace name subresource =
    Live.delete_subresource ?cancel ?options reader.client ?namespace name
      subresource

  let stream_subresource ?cancel ?query ?max_error_body_bytes reader ?namespace
      name subresource ~on_chunk =
    Live.stream_subresource ?cancel ?query ?max_error_body_bytes reader.client
      ?namespace name subresource ~on_chunk

  let get_scale ?cancel reader ?namespace name =
    Live.get_scale ?cancel reader.client ?namespace name

  let replace_scale ?cancel ?options reader ?namespace name scale =
    Live.replace_scale ?cancel ?options reader.client ?namespace name scale

  let patch_scale ?cancel ?options reader ?namespace name patch =
    Live.patch_scale ?cancel ?options reader.client ?namespace name patch

  let logs ?cancel ?options ?max_body_bytes reader ?namespace name =
    Live.logs ?cancel ?options ?max_body_bytes reader.client ?namespace name

  let stream_logs ?cancel ?options ?max_error_body_bytes reader ?namespace name
      ~on_chunk =
    Live.stream_logs ?cancel ?options ?max_error_body_bytes reader.client
      ?namespace name ~on_chunk
end
