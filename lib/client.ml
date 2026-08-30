module Transport = struct
  type request = {
    cancel : Cancel.t option;
    meth : Http.meth;
    target : string;
    headers : (string * string) list;
    body : string option;
    on_chunk : (string -> unit) option;
    max_body_bytes : int option;
  }

  type t = {
    execute_fn : request -> (Http.response, string) result;
    close_fn : unit -> unit;
    closed_error : string;
    closed : bool Atomic.t;
  }

  let make_with_closed_error ?(close = fun () -> ()) ~closed_error execute_fn =
    { execute_fn; close_fn = close; closed_error; closed = Atomic.make false }

  let make ?close execute_fn =
    make_with_closed_error ?close ~closed_error:"client transport is closed"
      execute_fn

  let execute transport request =
    if Atomic.get transport.closed then Error transport.closed_error
    else if Option.fold ~none:false ~some:Cancel.is_cancelled request.cancel
    then Error "request cancelled"
    else
      let limit =
        Option.value ~default:(32 * 1024 * 1024) request.max_body_bytes
      in
      if limit < 0 then Error "max_body_bytes must not be negative"
      else
        match
          try transport.execute_fn request
          with exn ->
            Error ("client transport raised: " ^ Printexc.to_string exn)
        with
        | Ok response when String.length response.Http.body > limit ->
            Error (Printf.sprintf "HTTP body exceeds %d bytes" limit)
        | result -> result

  let close transport =
    if Atomic.compare_and_set transport.closed false true then
      transport.close_fn ()
end

type t = {
  config : Config.t;
  transport : Transport.t;
  rate_limiter : Rate_limiter.t;
  logger : Log.t;
  next_request_id : int Atomic.t;
}

type api_error = {
  code : int;
  reason : string option;
  message : string;
  retry_after_seconds : int option;
  body : Yojson.Safe.t option;
}

type error =
  | Transport of string
  | Api of api_error
  | Decode of string
  | Invalid_request of string

module Error = struct
  let status_code = function
    | Api error -> Some error.code
    | _ -> None

  let reason = function
    | Api error -> error.reason
    | _ -> None

  let known_reasons =
    [
      "Unauthorized";
      "Forbidden";
      "NotFound";
      "AlreadyExists";
      "Conflict";
      "Gone";
      "Invalid";
      "ServerTimeout";
      "Timeout";
      "TooManyRequests";
      "BadRequest";
      "MethodNotAllowed";
      "NotAcceptable";
      "RequestEntityTooLarge";
      "UnsupportedMediaType";
      "InternalError";
      "Expired";
      "ServiceUnavailable";
      "StoreReadError";
    ]

  let reason_is_known reason = List.mem reason known_reasons

  let matches ~reason ~code = function
    | Api error -> (
        match error.reason with
        | Some candidate when candidate = reason -> true
        | Some candidate when reason_is_known candidate -> false
        | None | Some _ -> error.code = code)
    | Transport _ | Decode _ | Invalid_request _ -> false

  let is_unauthorized = matches ~reason:"Unauthorized" ~code:401
  let is_forbidden = matches ~reason:"Forbidden" ~code:403
  let is_not_found = matches ~reason:"NotFound" ~code:404
  let is_already_exists = matches ~reason:"AlreadyExists" ~code:409
  let is_conflict = matches ~reason:"Conflict" ~code:409
  let is_gone = matches ~reason:"Gone" ~code:410
  let is_resource_expired = matches ~reason:"Expired" ~code:410
  let is_invalid = matches ~reason:"Invalid" ~code:422
  let is_bad_request = matches ~reason:"BadRequest" ~code:400
  let is_method_not_supported = matches ~reason:"MethodNotAllowed" ~code:405
  let is_not_acceptable = matches ~reason:"NotAcceptable" ~code:406

  let is_request_entity_too_large =
    matches ~reason:"RequestEntityTooLarge" ~code:413

  let is_unsupported_media_type =
    matches ~reason:"UnsupportedMediaType" ~code:415

  let is_timeout = matches ~reason:"Timeout" ~code:504

  let is_server_timeout = function
    | Api { reason = Some "ServerTimeout"; _ } -> true
    | _ -> false

  let is_too_many_requests = matches ~reason:"TooManyRequests" ~code:429
  let is_internal_error = matches ~reason:"InternalError" ~code:500
  let is_service_unavailable = matches ~reason:"ServiceUnavailable" ~code:503

  let suggested_delay = function
    | Api { retry_after_seconds = Some seconds; _ } ->
        Some (float_of_int seconds)
    | error when is_server_timeout error -> Some 0.
    | Transport _ | Api _ | Decode _ | Invalid_request _ -> None

  let is_transient = function
    | Transport _ -> true
    | Api _ as error -> (
        is_timeout error || is_server_timeout error
        || is_too_many_requests error || is_internal_error error
        || is_service_unavailable error
        ||
        match status_code error with
        | Some (408 | 425 | 502 | 504) -> true
        | Some code when code >= 500 && code <= 599 -> true
        | Some _ | None -> false)
    | Decode _ | Invalid_request _ -> false
end

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

type scale_spec = { replicas : int32 }
type scale_status = { replicas : int32; selector : string option }

type scale = {
  api_version : string;
  kind : string;
  metadata : Core.object_meta;
  spec : scale_spec;
  status : scale_status option;
}

type log_stream = [ `All | `Stdout | `Stderr ]

type log_options = {
  container : string option;
  follow : bool;
  previous : bool;
  since_seconds : int option;
  since_time : string option;
  timestamps : bool;
  tail_lines : int64 option;
  limit_bytes : int64 option;
  insecure_skip_tls_verify_backend : bool;
  stream : log_stream option;
}

let default_log_options =
  {
    container = None;
    follow = false;
    previous = false;
    since_seconds = None;
    since_time = None;
    timestamps = false;
    tail_lines = None;
    limit_bytes = None;
    insecure_skip_tls_verify_backend = false;
    stream = None;
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

let create_with_transport ?rate_limiter ?(logger = Log.null) ~transport config =
  let rate_limiter =
    match rate_limiter with
    | Some rate_limiter -> rate_limiter
    | None -> Rate_limiter.create ~qps:20.0 ~burst:30
  in
  { config; transport; rate_limiter; logger; next_request_id = Atomic.make 0 }

let create ?max_idle_connections ?connect_timeout ?write_timeout
    ?response_header_timeout ?rate_limiter ?(logger = Log.null) config =
  let http =
    Http.create ?max_idle_connections ?connect_timeout ?write_timeout
      ?response_header_timeout config
  in
  let transport =
    Transport.make_with_closed_error ~closed_error:"HTTP transport is closed"
      ~close:(fun () -> Http.close http)
      (fun request ->
        Http.request ?cancel:request.cancel ~headers:request.headers
          ?body:request.body ?on_chunk:request.on_chunk
          ?max_body_bytes:request.max_body_bytes http request.meth
          request.target)
  in
  create_with_transport ?rate_limiter ~logger ~transport config

let close client = Transport.close client.transport
let config client = client.config
let logger client = client.logger

let pp_error formatter = function
  | Transport message -> Format.fprintf formatter "transport error: %s" message
  | Decode message -> Format.fprintf formatter "decode error: %s" message
  | Invalid_request message ->
      Format.fprintf formatter "invalid request: %s" message
  | Api error ->
      Format.fprintf formatter "Kubernetes API error %d%s%s: %s" error.code
        (match error.reason with
        | None -> ""
        | Some reason -> " " ^ reason)
        (match error.retry_after_seconds with
        | None -> ""
        | Some seconds -> Printf.sprintf " (retry after %ds)" seconds)
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

let json_int64 = function
  | `Int value -> Some (Int64.of_int value)
  | `Intlit value -> Int64.of_string_opt value
  | _ -> None

let int32_of_json context json =
  match json_int64 json with
  | Some value
    when Int64.compare value (Int64.of_int32 Int32.min_int) >= 0
         && Int64.compare value (Int64.of_int32 Int32.max_int) <= 0 ->
      Ok (Int64.to_int32 value)
  | Some _ -> Error (context ^ " is outside the int32 range")
  | None -> Error (context ^ " must be an integer")

let scale_of_json json =
  let required_string context name =
    match Option.bind (json_member name json) json_string with
    | Some value -> Ok value
    | None -> Error (context ^ " is required")
  in
  let* api_version = required_string "Scale.apiVersion" "apiVersion" in
  let* () =
    if api_version = "autoscaling/v1" then Ok ()
    else Error ("unsupported Scale apiVersion " ^ api_version)
  in
  let* kind = required_string "Scale.kind" "kind" in
  let* () =
    if kind = "Scale" then Ok () else Error ("unexpected Scale kind " ^ kind)
  in
  let* metadata = Core.object_meta_of_json json in
  let* spec =
    match json_member "spec" json with
    | Some (`Assoc _ as spec) -> (
        match json_member "replicas" spec with
        | Some value ->
            let* replicas = int32_of_json "Scale.spec.replicas" value in
            Ok { replicas }
        | None -> Ok { replicas = 0l })
    | Some _ -> Error "Scale.spec must be an object"
    | None -> Error "Scale.spec is required"
  in
  let* status =
    match json_member "status" json with
    | None | Some `Null -> Ok None
    | Some (`Assoc _ as status) -> (
        match json_member "replicas" status with
        | None -> Error "Scale.status.replicas is required"
        | Some value ->
            let* replicas = int32_of_json "Scale.status.replicas" value in
            let* selector =
              match json_member "selector" status with
              | None | Some `Null -> Ok None
              | Some (`String value) -> Ok (Some value)
              | Some _ -> Error "Scale.status.selector must be a string"
            in
            Ok (Some { replicas; selector }))
    | Some _ -> Error "Scale.status must be an object"
  in
  Ok { api_version; kind; metadata; spec; status }

let scale_to_json scale =
  let status =
    match scale.status with
    | None -> []
    | Some status ->
        [
          ( "status",
            `Assoc
              ([ ("replicas", `Int (Int32.to_int status.replicas)) ]
              @
              match status.selector with
              | None -> []
              | Some selector -> [ ("selector", `String selector) ]) );
        ]
  in
  `Assoc
    ([
       ("apiVersion", `String scale.api_version);
       ("kind", `String scale.kind);
       ("metadata", Core.object_meta_to_json scale.metadata);
       ("spec", `Assoc [ ("replicas", `Int (Int32.to_int scale.spec.replicas)) ]);
     ]
    @ status)

let retry_after_of_status body =
  Option.bind body (json_member "details") |> fun details ->
  Option.bind details (json_member "retryAfterSeconds") |> fun value ->
  Option.bind value json_int |> fun seconds ->
  Option.bind seconds (fun seconds ->
      if seconds < 0 then None else Some seconds)

let retry_after_of_headers headers =
  List.find_map
    (fun (name, value) ->
      if String.lowercase_ascii name <> "retry-after" then None
      else
        match int_of_string_opt (String.trim value) with
        | Some seconds when seconds >= 0 -> Some seconds
        | Some _ | None -> None)
    headers

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
  let retry_after_seconds =
    match retry_after_of_headers response.headers with
    | Some _ as value -> value
    | None -> retry_after_of_status body
  in
  { code; reason; message; retry_after_seconds; body }

let raw_unlogged ?cancel ?(headers = []) ?body ?on_chunk ?max_body_bytes client
    meth path =
  let* () =
    if Rate_limiter.acquire ?cancel client.rate_limiter then Ok ()
    else Error (Transport "request cancelled while waiting for rate limiter")
  in
  let authorization () =
    match Config.authorization_header client.config with
    | Ok value -> Ok value
    | Error message -> Error (Transport message)
  in
  let headers =
    if
      List.exists
        (fun (name, _) -> String.lowercase_ascii name = "impersonate-user")
        headers
    then headers
    else Config.impersonation_headers client.config @ headers
  in
  let perform authorization =
    let headers =
      match authorization with
      | None -> headers
      | Some value -> ("Authorization", value) :: headers
    in
    match
      Transport.execute client.transport
        { cancel; meth; target = path; headers; body; on_chunk; max_body_bytes }
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

let method_string = function
  | `GET -> "GET"
  | `POST -> "POST"
  | `PUT -> "PUT"
  | `PATCH -> "PATCH"
  | `DELETE -> "DELETE"

let target_path value =
  try Uri.path (Uri.of_string value) with _ -> "<invalid>"

let raw ?cancel ?headers ?body ?on_chunk ?max_body_bytes client meth path =
  if not (Log.enabled client.logger Log.Debug) then
    raw_unlogged ?cancel ?headers ?body ?on_chunk ?max_body_bytes client meth
      path
  else
    let request_id = Atomic.fetch_and_add client.next_request_id 1 in
    let started = Clock.now () in
    let common =
      [
        ("request_id", Log.Int request_id);
        ("method", Log.String (method_string meth));
        ("path", Log.String (target_path path));
      ]
    in
    Log.debug client.logger ~fields:common "Kubernetes API request started";
    let result =
      raw_unlogged ?cancel ?headers ?body ?on_chunk ?max_body_bytes client meth
        path
    in
    let outcome_fields =
      match result with
      | Ok response ->
          [
            ("result", Log.String "ok");
            ("status_code", Log.Int response.Http.status);
          ]
      | Error (Api error) ->
          [
            ("result", Log.String "api_error");
            ("status_code", Log.Int error.code);
          ]
      | Error (Transport _) -> [ ("result", Log.String "transport_error") ]
      | Error (Decode _) -> [ ("result", Log.String "decode_error") ]
      | Error (Invalid_request _) ->
          [ ("result", Log.String "invalid_request") ]
    in
    Log.debug client.logger
      ~fields:
        (common
        @ ("duration_seconds", Log.Float (Clock.elapsed started))
          :: outcome_fields)
      "Kubernetes API request completed";
    result

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

  let validate_delete_options (options : delete_options) =
    match options.grace_period_seconds with
    | Some value when value < 0 ->
        Error
          (Invalid_request "delete grace_period_seconds must not be negative")
    | _ -> Ok ()

  let propagation_policy_string = function
    | `Orphan -> "Orphan"
    | `Background -> "Background"
    | `Foreground -> "Foreground"

  let optional_json name fn = function
    | None -> []
    | Some value -> [ (name, fn value) ]

  let delete_options_json (options : delete_options) =
    let preconditions =
      optional_json "uid" (fun value -> `String value) options.precondition_uid
      @ optional_json "resourceVersion"
          (fun value -> `String (Core.Resource_version.to_string value))
          options.precondition_resource_version
    in
    `Assoc
      ([ ("apiVersion", `String "v1"); ("kind", `String "DeleteOptions") ]
      @ optional_json "gracePeriodSeconds"
          (fun value -> `Int value)
          options.grace_period_seconds
      @ optional_json "propagationPolicy"
          (fun value -> `String (propagation_policy_string value))
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

  let delete_request ?cancel client path options =
    let* () = validate_delete_options options in
    let* _response =
      raw ?cancel
        ~headers:[ ("Content-Type", "application/json") ]
        ~body:(encode_json (delete_options_json options))
        client `DELETE path
    in
    Ok ()

  let delete ?cancel ?(options = default_delete_options) client ?namespace name
      =
    let namespace = object_namespace client namespace in
    let* path = path (Core.object_path Resource.api ~namespace ~name) in
    delete_request ?cancel client path options

  let delete_collection ?cancel ?(options = default_delete_options) ?namespace
      ?(all_namespaces = false) ?label_selector ?field_selector
      ?resource_version ?resource_version_match ?limit ?continue
      ?timeout_seconds client =
    let* () =
      match limit with
      | Some value when value < 0 ->
          Error (Invalid_request "delete_collection limit must not be negative")
      | _ -> Ok ()
    in
    let* () =
      match timeout_seconds with
      | Some value when value < 0 ->
          Error
            (Invalid_request
               "delete_collection timeout_seconds must not be negative")
      | _ -> Ok ()
    in
    let* namespace =
      match (Resource.api.scope, all_namespaces, namespace) with
      | Core.Namespaced, true, None -> Ok None
      | Core.Namespaced, true, Some _ ->
          Error
            (Invalid_request
               "delete_collection cannot combine namespace and all_namespaces")
      | Core.Namespaced, false, namespace ->
          Ok (object_namespace client namespace)
      | Core.Cluster, true, _ ->
          Error
            (Invalid_request
               "all_namespaces is invalid for a cluster-scoped resource")
      | Core.Cluster, false, namespace -> Ok namespace
    in
    let* base_path = path (Core.collection_path Resource.api ~namespace) in
    let query =
      ( ( ( ( ( ( [] |> fun values ->
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
        | Some value -> ("continue", value) :: values )
      |> fun values ->
      match timeout_seconds with
      | None -> values
      | Some value -> ("timeoutSeconds", string_of_int value) :: values
    in
    delete_request ?cancel client (with_query base_path query) options

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

  let subresource_request_path client namespace name subresource =
    if String.trim subresource = "" then
      Error (Invalid_request "subresource must not be empty")
    else
      let namespace = object_namespace client namespace in
      path (Core.subresource_path Resource.api ~namespace ~name ~subresource)

  let get_subresource ?cancel client ?namespace name subresource =
    let* path = subresource_request_path client namespace name subresource in
    let* response = raw ?cancel client `GET path in
    decode_json response.body

  let write_subresource_json ?cancel ?(options = default_write_options) client
      ?namespace ~name ~subresource meth value =
    let* path = subresource_request_path client namespace name subresource in
    let* response =
      raw ?cancel
        ~headers:[ ("Content-Type", "application/json") ]
        ~body:(encode_json value) client meth
        (with_write_options path options)
    in
    decode_json response.body

  let create_subresource ?cancel ?options client ?namespace ~name ~subresource
      value =
    write_subresource_json ?cancel ?options client ?namespace ~name ~subresource
      `POST value

  let replace_subresource ?cancel ?options client ?namespace ~name ~subresource
      value =
    write_subresource_json ?cancel ?options client ?namespace ~name ~subresource
      `PUT value

  let patch_subresource ?cancel ?options client ?namespace ~name ~subresource
      value =
    if String.trim subresource = "" then
      Error (Invalid_request "subresource must not be empty")
    else
      patch_request_json ?cancel ?options client ?namespace name ~subresource
        value

  let delete_subresource ?cancel ?(options = default_delete_options) client
      ?namespace name subresource =
    let* path = subresource_request_path client namespace name subresource in
    delete_request ?cancel client path options

  let stream_subresource ?cancel ?(query = [])
      ?(max_error_body_bytes = 32 * 1024 * 1024) client ?namespace name
      subresource ~on_chunk =
    let* () =
      if max_error_body_bytes < 0 then
        Error (Invalid_request "max_error_body_bytes must not be negative")
      else Ok ()
    in
    let* path = subresource_request_path client namespace name subresource in
    let* _response =
      raw ?cancel
        ~headers:[ ("Accept", "*/*") ]
        ~on_chunk ~max_body_bytes:max_error_body_bytes client `GET
        (with_query path query)
    in
    Ok ()

  let decode_scale_json json =
    match scale_of_json json with
    | Ok scale -> Ok scale
    | Error message -> Error (Decode message)

  let get_scale ?cancel client ?namespace name =
    let* json = get_subresource ?cancel client ?namespace name "scale" in
    decode_scale_json json

  let replace_scale ?cancel ?options client ?namespace name scale =
    let* json =
      replace_subresource ?cancel ?options client ?namespace ~name
        ~subresource:"scale" (scale_to_json scale)
    in
    decode_scale_json json

  let patch_scale ?cancel ?options client ?namespace name value =
    let* json =
      patch_subresource ?cancel ?options client ?namespace ~name
        ~subresource:"scale" value
    in
    decode_scale_json json

  let log_stream_string = function
    | `All -> "All"
    | `Stdout -> "Stdout"
    | `Stderr -> "Stderr"

  let log_query (options : log_options) =
    let* () =
      match options.container with
      | Some value when String.trim value = "" ->
          Error (Invalid_request "log container must not be empty")
      | _ -> Ok ()
    in
    let* () =
      match (options.since_seconds, options.since_time) with
      | Some _, Some _ ->
          Error
            (Invalid_request
               "log since_seconds and since_time are mutually exclusive")
      | Some value, None when value < 1 ->
          Error (Invalid_request "log since_seconds must be positive")
      | _ -> Ok ()
    in
    let* () =
      match options.since_time with
      | Some value when String.trim value = "" ->
          Error (Invalid_request "log since_time must not be empty")
      | _ -> Ok ()
    in
    let* () =
      match options.tail_lines with
      | Some value when Int64.compare value 0L < 0 ->
          Error (Invalid_request "log tail_lines must not be negative")
      | _ -> Ok ()
    in
    let* () =
      match options.limit_bytes with
      | Some value when Int64.compare value 0L <= 0 ->
          Error (Invalid_request "log limit_bytes must be positive")
      | _ -> Ok ()
    in
    let optional name fn = function
      | None -> []
      | Some value -> [ (name, fn value) ]
    in
    Ok
      (optional "container" Fun.id options.container
      @ (if options.follow then [ ("follow", "true") ] else [])
      @ (if options.previous then [ ("previous", "true") ] else [])
      @ optional "sinceSeconds" string_of_int options.since_seconds
      @ optional "sinceTime" Fun.id options.since_time
      @ (if options.timestamps then [ ("timestamps", "true") ] else [])
      @ optional "tailLines" Int64.to_string options.tail_lines
      @ optional "limitBytes" Int64.to_string options.limit_bytes
      @ (if options.insecure_skip_tls_verify_backend then
           [ ("insecureSkipTLSVerifyBackend", "true") ]
         else [])
      @ optional "stream" log_stream_string options.stream)

  let logs ?cancel ?(options = default_log_options) ?max_body_bytes client
      ?namespace name =
    let* query = log_query options in
    let* path = subresource_request_path client namespace name "log" in
    let* response =
      raw ?cancel
        ~headers:[ ("Accept", "text/plain, */*") ]
        ?max_body_bytes client `GET (with_query path query)
    in
    Ok response.body

  let stream_logs ?cancel ?(options = default_log_options) ?max_error_body_bytes
      client ?namespace name ~on_chunk =
    let* query = log_query options in
    stream_subresource ?cancel ~query ?max_error_body_bytes client ?namespace
      name "log" ~on_chunk

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
    let retry_after_seconds = retry_after_of_status (Some json) in
    { code; reason; message; retry_after_seconds; body = Some json }

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
    let terminal_error = ref None in
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
                  if Error.is_resource_expired (Api error) then expired := true
                  else (
                    terminal_error := Some error;
                    on_event (Watch_error error))
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
      let oversize () =
        parse_error :=
          Some
            (Decode
               (Printf.sprintf "watch event exceeds %d bytes" max_event_bytes));
        raise (Failure "watch event exceeds configured limit")
      in
      let contents = Buffer.contents line_buffer in
      let rec split start =
        match String.index_from_opt contents start '\n' with
        | None ->
            let length = String.length contents - start in
            if length > max_event_bytes then oversize ();
            let remainder = String.sub contents start length in
            Buffer.clear line_buffer;
            Buffer.add_string line_buffer remainder
        | Some ending ->
            if ending - start > max_event_bytes then oversize ();
            process_line (String.sub contents start (ending - start));
            if !parse_error <> None then raise (Failure "invalid watch event");
            if !expired || !terminal_error <> None then
              raise (Failure "watch returned a Status error");
            split (ending + 1)
      in
      split 0
    in
    match
      raw ?cancel ~on_chunk:consume client `GET (with_query base_path query)
    with
    | Error _ when !parse_error <> None -> Error (Option.get !parse_error)
    | Error _ when !expired -> Ok Resource_version_expired
    | Error _ when !terminal_error <> None ->
        Error (Api (Option.get !terminal_error))
    | Error (Api _ as error) when Error.is_resource_expired error ->
        Ok Resource_version_expired
    | Error _ as error -> error
    | Ok _ -> (
        if Buffer.length line_buffer > 0 then
          process_line (Buffer.contents line_buffer);
        match !parse_error with
        | Some error -> Error error
        | None when !expired -> Ok Resource_version_expired
        | None when !terminal_error <> None ->
            Error (Api (Option.get !terminal_error))
        | None -> Ok (Watch_ended !latest))
end
