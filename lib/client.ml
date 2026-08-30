type t = { config : Config.t }

type api_error = {
  code : int;
  reason : string option;
  message : string;
  body : Yojson.Safe.t option;
}

type error =
  | Transport of string
  | Api of api_error
  | Decode of string
  | Invalid_request of string

type patch =
  | Json_patch of Yojson.Safe.t
  | Merge_patch of Yojson.Safe.t
  | Apply of { value : Yojson.Safe.t; field_manager : string; force : bool }

type field_validation = [ `Ignore | `Warn | `Strict ]

type write_options = {
  dry_run : string list;
  field_manager : string option;
  field_validation : field_validation option;
}

let default_write_options =
  { dry_run = []; field_manager = None; field_validation = None }

type propagation_policy = [ `Orphan | `Background | `Foreground ]

type delete_options = {
  grace_period_seconds : int option;
  propagation_policy : propagation_policy option;
  precondition_uid : string option;
  precondition_resource_version : Core.Resource_version.t option;
  dry_run : string list;
}

let default_delete_options =
  {
    grace_period_seconds = None;
    propagation_policy = None;
    precondition_uid = None;
    precondition_resource_version = None;
    dry_run = [];
  }

type 'a list_result = {
  items : 'a list;
  resource_version : Core.Resource_version.t;
  continue_token : string option;
  remaining_item_count : int option;
}

type 'a watch_event =
  | Added of 'a
  | Modified of 'a
  | Deleted of 'a
  | Bookmark of Core.Resource_version.t option
  | Watch_error of api_error

type watch_outcome =
  | Watch_ended of Core.Resource_version.t
  | Resource_version_expired

let ( let* ) result fn =
  match result with
  | Ok value -> fn value
  | Error _ as error -> error

let create config = { config }
let config client = client.config

let pp_error formatter = function
  | Transport message -> Format.fprintf formatter "transport error: %s" message
  | Decode message -> Format.fprintf formatter "decode error: %s" message
  | Invalid_request message ->
      Format.fprintf formatter "invalid request: %s" message
  | Api error ->
      Format.fprintf formatter "Kubernetes API error %d%s: %s" error.code
        (match error.reason with
        | None -> ""
        | Some reason -> " " ^ reason)
        error.message

let json_member name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let json_string = function
  | `String value -> Some value
  | _ -> None

let json_int = function
  | `Int value -> Some value
  | `Intlit value -> int_of_string_opt value
  | _ -> None

let api_error_of_response response =
  let body =
    try Some (Yojson.Safe.from_string response.Http.body)
    with Yojson.Json_error _ -> None
  in
  let reason =
    Option.bind (Option.bind body (json_member "reason")) json_string
  in
  let message =
    Option.bind (Option.bind body (json_member "message")) json_string
    |> Option.value
         ~default:
           (if response.reason = "" then response.body else response.reason)
  in
  let code =
    Option.bind (Option.bind body (json_member "code")) json_int
    |> Option.value ~default:response.status
  in
  { code; reason; message; body }

let raw ?cancel ?(headers = []) ?body ?on_chunk ?max_body_bytes client meth path
    =
  let authorization () =
    match Config.authorization_header client.config with
    | Ok value -> Ok value
    | Error message -> Error (Transport message)
  in
  let perform authorization =
    let headers =
      match authorization with
      | None -> headers
      | Some value -> ("Authorization", value) :: headers
    in
    match
      Http.request ?cancel ~headers ?body ?on_chunk ?max_body_bytes
        client.config meth path
    with
    | Error message -> Error (Transport message)
    | Ok response -> Ok response
  in
  let* first_authorization = authorization () in
  let* response = perform first_authorization in
  let* response =
    if response.status = 401 && Config.invalidate_credential client.config then
      let* refreshed_authorization = authorization () in
      perform refreshed_authorization
    else Ok response
  in
  match response with
  | response when response.status >= 200 && response.status < 300 -> Ok response
  | response -> Error (Api (api_error_of_response response))

let with_query path query =
  Uri.of_string path |> fun uri -> Uri.with_query' uri query |> Uri.to_string

let encode_json value = Yojson.Safe.to_string value

module For (Resource : Core.Resource) = struct
  let resource_version_match_string = function
    | `Exact -> "Exact"
    | `Not_older_than -> "NotOlderThan"

  let decode body =
    try
      let json = Yojson.Safe.from_string body in
      match Resource.of_json json with
      | Ok value -> Ok value
      | Error message -> Error (Decode message)
    with Yojson.Json_error message -> Error (Decode message)

  let path result =
    match result with
    | Ok value -> Ok value
    | Error message -> Error (Invalid_request message)

  let object_namespace client explicit =
    match Resource.api.scope with
    | Core.Cluster -> explicit
    | Core.Namespaced ->
        Some
          (match (explicit, (config client).namespace) with
          | Some value, _ -> value
          | None, Some value -> value
          | None, None -> "default")

  let create_namespace client explicit value =
    match Resource.api.scope with
    | Core.Cluster -> explicit
    | Core.Namespaced ->
        let metadata = Resource.metadata value in
        Some
          (match (explicit, metadata.namespace, (config client).namespace) with
          | Some value, _, _ -> value
          | None, Some value, _ -> value
          | None, None, Some value -> value
          | None, None, None -> "default")

  let field_validation_string = function
    | `Ignore -> "Ignore"
    | `Warn -> "Warn"
    | `Strict -> "Strict"

  let write_query (options : write_options) =
    List.map (fun value -> ("dryRun", value)) options.dry_run
    @ (match options.field_manager with
      | None -> []
      | Some value -> [ ("fieldManager", value) ])
    @
    match options.field_validation with
    | None -> []
    | Some value -> [ ("fieldValidation", field_validation_string value) ]

  let with_write_options path options =
    let query = write_query options in
    if query = [] then path else with_query path query

  let decode_json body =
    try Ok (Yojson.Safe.from_string body)
    with Yojson.Json_error message -> Error (Decode message)

  let get ?cancel ?resource_version ?resource_version_match client ?namespace
      name =
    let namespace = object_namespace client namespace in
    let* path = path (Core.object_path Resource.api ~namespace ~name) in
    let query =
      ( [] |> fun values ->
        match resource_version with
        | None -> values
        | Some value ->
            ("resourceVersion", Core.Resource_version.to_string value) :: values
      )
      |> fun values ->
      match resource_version_match with
      | None -> values
      | Some value ->
          ("resourceVersionMatch", resource_version_match_string value)
          :: values
    in
    let* response = raw ?cancel client `GET (with_query path query) in
    decode response.body

  let create ?cancel ?(options = default_write_options) client ?namespace value
      =
    let namespace = create_namespace client namespace value in
    let* path = path (Core.collection_path Resource.api ~namespace) in
    let* response =
      raw ?cancel
        ~headers:[ ("Content-Type", "application/json") ]
        ~body:(encode_json (Resource.to_json value))
        client `POST
        (with_write_options path options)
    in
    decode response.body

  let replace ?cancel ?(options = default_write_options) client ?namespace name
      value =
    let namespace = object_namespace client namespace in
    let* path = path (Core.object_path Resource.api ~namespace ~name) in
    let* response =
      raw ?cancel
        ~headers:[ ("Content-Type", "application/json") ]
        ~body:(encode_json (Resource.to_json value))
        client `PUT
        (with_write_options path options)
    in
    decode response.body

  let delete ?cancel ?(options = default_delete_options) client ?namespace name
      =
    let* () =
      match options.grace_period_seconds with
      | Some value when value < 0 ->
          Error
            (Invalid_request "delete grace_period_seconds must not be negative")
      | _ -> Ok ()
    in
    let namespace = object_namespace client namespace in
    let* path = path (Core.object_path Resource.api ~namespace ~name) in
    let propagation_policy = function
      | `Orphan -> "Orphan"
      | `Background -> "Background"
      | `Foreground -> "Foreground"
    in
    let optional name fn = function
      | None -> []
      | Some value -> [ (name, fn value) ]
    in
    let preconditions =
      optional "uid" (fun value -> `String value) options.precondition_uid
      @ optional "resourceVersion"
          (fun value -> `String (Core.Resource_version.to_string value))
          options.precondition_resource_version
    in
    let body =
      `Assoc
        ([ ("apiVersion", `String "v1"); ("kind", `String "DeleteOptions") ]
        @ optional "gracePeriodSeconds"
            (fun value -> `Int value)
            options.grace_period_seconds
        @ optional "propagationPolicy"
            (fun value -> `String (propagation_policy value))
            options.propagation_policy
        @ (if preconditions = [] then []
           else [ ("preconditions", `Assoc preconditions) ])
        @
        if options.dry_run = [] then []
        else
          [
            ( "dryRun",
              `List (List.map (fun value -> `String value) options.dry_run) );
          ])
    in
    let* _response =
      raw ?cancel
        ~headers:[ ("Content-Type", "application/json") ]
        ~body:(encode_json body) client `DELETE path
    in
    Ok ()

  let patch_request_json ?cancel ?(options = default_write_options) client
      ?namespace name ?subresource patch_value =
    let namespace = object_namespace client namespace in
    let* base_path =
      match subresource with
      | None -> path (Core.object_path Resource.api ~namespace ~name)
      | Some subresource ->
          path
            (Core.subresource_path Resource.api ~namespace ~name ~subresource)
    in
    let content_type, json, patch_query =
      match patch_value with
      | Json_patch value -> ("application/json-patch+json", value, [])
      | Merge_patch value -> ("application/merge-patch+json", value, [])
      | Apply { value; field_manager = _; force } ->
          ( "application/apply-patch+yaml",
            value,
            [ ("force", string_of_bool force) ] )
    in
    let query =
      match patch_value with
      | Apply { field_manager; _ } ->
          ("fieldManager", field_manager)
          :: (List.remove_assoc "fieldManager" (write_query options)
             @ patch_query)
      | _ -> write_query options @ patch_query
    in
    let request_path =
      if query = [] then base_path else with_query base_path query
    in
    let* response =
      raw ?cancel
        ~headers:[ ("Content-Type", content_type) ]
        ~body:(encode_json json) client `PATCH request_path
    in
    decode_json response.body

  let decode_resource_json json =
    match Resource.of_json json with
    | Ok value -> Ok value
    | Error message -> Error (Decode message)

  let patch ?cancel ?options client ?namespace name value =
    let* json =
      patch_request_json ?cancel ?options client ?namespace name value
    in
    decode_resource_json json

  let patch_status ?cancel ?options client ?namespace name value =
    let* json =
      patch_request_json ?cancel ?options client ?namespace name
        ~subresource:"status" value
    in
    decode_resource_json json

  let replace_status ?cancel ?(options = default_write_options) client
      ?namespace name value =
    let namespace = object_namespace client namespace in
    let* path =
      path
        (Core.subresource_path Resource.api ~namespace ~name
           ~subresource:"status")
    in
    let* response =
      raw ?cancel
        ~headers:[ ("Content-Type", "application/json") ]
        ~body:(encode_json (Resource.to_json value))
        client `PUT
        (with_write_options path options)
    in
    decode response.body

  let get_subresource ?cancel client ?namespace name subresource =
    let namespace = object_namespace client namespace in
    let* path =
      path (Core.subresource_path Resource.api ~namespace ~name ~subresource)
    in
    let* response = raw ?cancel client `GET path in
    decode_json response.body

  let patch_subresource ?cancel ?options client ?namespace ~name ~subresource
      value =
    patch_request_json ?cancel ?options client ?namespace name ~subresource
      value

  let list ?cancel ?namespace ?label_selector ?field_selector ?resource_version
      ?resource_version_match ?limit ?continue client =
    let* () =
      match limit with
      | Some value when value < 0 ->
          Error (Invalid_request "list limit must not be negative")
      | _ -> Ok ()
    in
    let* base_path = path (Core.collection_path Resource.api ~namespace) in
    let query =
      ( ( ( ( ( [] |> fun values ->
                match label_selector with
                | None -> values
                | Some value -> ("labelSelector", value) :: values )
            |> fun values ->
              match field_selector with
              | None -> values
              | Some value -> ("fieldSelector", value) :: values )
          |> fun values ->
            match resource_version with
            | None -> values
            | Some value ->
                ("resourceVersion", Core.Resource_version.to_string value)
                :: values )
        |> fun values ->
          match resource_version_match with
          | None -> values
          | Some value ->
              ("resourceVersionMatch", resource_version_match_string value)
              :: values )
      |> fun values ->
        match limit with
        | None -> values
        | Some value -> ("limit", string_of_int value) :: values )
      |> fun values ->
      match continue with
      | None -> values
      | Some value -> ("continue", value) :: values
    in
    let* response = raw ?cancel client `GET (with_query base_path query) in
    try
      let json = Yojson.Safe.from_string response.body in
      let* raw_items =
        match json_member "items" json with
        | Some (`List items) -> Ok items
        | _ -> Error (Decode "list response is missing items")
      in
      let rec decode_items accumulator = function
        | [] -> Ok (List.rev accumulator)
        | item :: rest -> (
            match Resource.of_json item with
            | Ok item -> decode_items (item :: accumulator) rest
            | Error message -> Error (Decode message))
      in
      let* items = decode_items [] raw_items in
      let* metadata =
        match json_member "metadata" json with
        | Some metadata -> Ok metadata
        | None -> Error (Decode "list response is missing metadata")
      in
      let* resource_version =
        match
          Option.bind (json_member "resourceVersion" metadata) json_string
        with
        | Some value -> Ok (Core.Resource_version.of_string value)
        | None -> Error (Decode "list metadata is missing resourceVersion")
      in
      let continue_token =
        Option.bind (json_member "continue" metadata) json_string
      in
      let remaining_item_count =
        Option.bind (json_member "remainingItemCount" metadata) json_int
      in
      Ok { items; resource_version; continue_token; remaining_item_count }
    with Yojson.Json_error message -> Error (Decode message)

  let list_all ?cancel ?namespace ?label_selector ?field_selector
      ?resource_version ?resource_version_match ?(page_size = 500) client =
    let* () =
      if page_size < 1 then
        Error (Invalid_request "list_all page_size must be positive")
      else Ok ()
    in
    let rec loop accumulator snapshot_version continue =
      let* page =
        list ?cancel ?namespace ?label_selector ?field_selector
          ?resource_version ?resource_version_match ~limit:page_size ?continue
          client
      in
      let snapshot_version =
        match snapshot_version with
        | None -> page.resource_version
        | Some value -> value
      in
      let* () =
        if
          Core.Resource_version.to_string snapshot_version
          <> Core.Resource_version.to_string page.resource_version
        then
          Error
            (Decode
               "paginated LIST changed resourceVersion between continuation \
                pages")
        else Ok ()
      in
      let accumulator = List.rev_append page.items accumulator in
      match page.continue_token with
      | Some token when Some token = continue ->
          Error (Decode "paginated LIST repeated its continuation token")
      | Some token when token <> "" ->
          loop accumulator (Some snapshot_version) (Some token)
      | _ ->
          Ok
            {
              items = List.rev accumulator;
              resource_version = snapshot_version;
              continue_token = None;
              remaining_item_count = Some 0;
            }
    in
    loop [] None None

  let status_error json =
    let reason = Option.bind (json_member "reason" json) json_string in
    let message =
      Option.bind (json_member "message" json) json_string
      |> Option.value ~default:"watch returned a Status error"
    in
    let code =
      Option.bind (json_member "code" json) json_int
      |> Option.value ~default:500
    in
    { code; reason; message; body = Some json }

  let resource_version_of_object json =
    Option.bind
      (Option.bind
         (json_member "metadata" json)
         (json_member "resourceVersion"))
      json_string
    |> Option.map Core.Resource_version.of_string

  let watch ?cancel ?namespace ?label_selector ?field_selector
      ?(timeout_seconds = 300) ?(allow_bookmarks = true) ?resource_version_match
      ?(send_initial_events = false) ?(max_event_bytes = 16 * 1024 * 1024)
      client ~resource_version ~on_event =
    let* () =
      if timeout_seconds < 0 then
        Error (Invalid_request "timeout_seconds must not be negative")
      else if max_event_bytes < 1 then
        Error (Invalid_request "max_event_bytes must be positive")
      else if
        send_initial_events && resource_version_match <> Some `Not_older_than
      then
        Error
          (Invalid_request
             "send_initial_events requires \
              resource_version_match=`Not_older_than")
      else Ok ()
    in
    let* base_path = path (Core.collection_path Resource.api ~namespace) in
    let query =
      ( ( ( [
              ("watch", "true");
              ("allowWatchBookmarks", string_of_bool allow_bookmarks);
              ( "resourceVersion",
                Core.Resource_version.to_string resource_version );
              ("timeoutSeconds", string_of_int timeout_seconds);
            ]
          |> fun values ->
            match label_selector with
            | None -> values
            | Some value -> ("labelSelector", value) :: values )
        |> fun values ->
          match field_selector with
          | None -> values
          | Some value -> ("fieldSelector", value) :: values )
      |> fun values ->
        match resource_version_match with
        | None -> values
        | Some value ->
            ("resourceVersionMatch", resource_version_match_string value)
            :: values )
      |> fun values ->
      if send_initial_events then ("sendInitialEvents", "true") :: values
      else values
    in
    let line_buffer = Buffer.create 4096 in
    let latest = ref resource_version in
    let parse_error = ref None in
    let expired = ref false in
    let process_line line =
      if !parse_error = None && String.trim line <> "" then
        try
          let json = Yojson.Safe.from_string line in
          match (json_member "type" json, json_member "object" json) with
          | Some (`String event_type), Some object_json -> (
              (match resource_version_of_object object_json with
              | Some value -> latest := value
              | None -> ());
              match event_type with
              | "BOOKMARK" ->
                  on_event (Bookmark (resource_version_of_object object_json))
              | "ERROR" ->
                  let error = status_error object_json in
                  if error.code = 410 then expired := true
                  else on_event (Watch_error error)
              | "ADDED" | "MODIFIED" | "DELETED" -> (
                  match Resource.of_json object_json with
                  | Error message -> parse_error := Some (Decode message)
                  | Ok value ->
                      on_event
                        (match event_type with
                        | "ADDED" -> Added value
                        | "MODIFIED" -> Modified value
                        | _ -> Deleted value))
              | _ -> ())
          | _ -> parse_error := Some (Decode "invalid watch event envelope")
        with Yojson.Json_error message -> parse_error := Some (Decode message)
    in
    let consume chunk =
      Buffer.add_string line_buffer chunk;
      if Buffer.length line_buffer > max_event_bytes then (
        parse_error :=
          Some
            (Decode
               (Printf.sprintf "watch event exceeds %d bytes" max_event_bytes));
        raise (Failure "watch event exceeds configured limit"));
      let contents = Buffer.contents line_buffer in
      let rec split start =
        match String.index_from_opt contents start '\n' with
        | None ->
            let remainder =
              String.sub contents start (String.length contents - start)
            in
            Buffer.clear line_buffer;
            Buffer.add_string line_buffer remainder
        | Some ending ->
            process_line (String.sub contents start (ending - start));
            if !parse_error <> None then raise (Failure "invalid watch event");
            split (ending + 1)
      in
      split 0
    in
    match
      raw ?cancel ~on_chunk:consume client `GET (with_query base_path query)
    with
    | Error _ when !parse_error <> None -> Error (Option.get !parse_error)
    | Error (Api error) when error.code = 410 -> Ok Resource_version_expired
    | Error _ as error -> error
    | Ok _ -> (
        if Buffer.length line_buffer > 0 then
          process_line (Buffer.contents line_buffer);
        match !parse_error with
        | Some error -> Error error
        | None when !expired -> Ok Resource_version_expired
        | None -> Ok (Watch_ended !latest))
end
