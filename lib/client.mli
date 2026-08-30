(** Typed Kubernetes HTTP operations, list pagination, patches, and watches. *)

type t

module Transport : sig
  type request = {
    cancel : Cancel.t option;
    meth : Http.meth;
    target : string;
    headers : (string * string) list;
    body : string option;
    on_chunk : (string -> unit) option;
    max_body_bytes : int option;
  }
  (** One fully prepared wire request. Authentication headers have already been
      resolved, but HTTP framing headers remain the transport's responsibility.
      A successful streaming response should call [on_chunk] without retaining
      those bytes in the returned response body. Error bodies must always remain
      bounded by [max_body_bytes]. *)

  type websocket_request = {
    cancel : Cancel.t option;
    target : string;
    headers : (string * string) list;
    protocols : string list;
    max_message_bytes : int option;
    max_error_body_bytes : int option;
  }

  type t

  val make :
    ?close:(unit -> unit) ->
    ?websocket:
      (websocket_request -> (Websocket.t, Websocket.connect_error) result) ->
    (request -> (Http.response, string) result) ->
    t
  (** Build a custom transport. The callback may be invoked concurrently and
      must be thread-safe, honor cancellation while it is running, enforce
      bounds before invoking streaming callbacks, and preserve the streaming
      semantics described by the request. The close callback may run while
      requests are in flight and must also be thread-safe. The wrapper rejects
      requests already cancelled, validates returned buffered-body bounds, and
      converts callback exceptions to transport errors. *)

  val execute : t -> request -> (Http.response, string) result
  (** Execute a prepared request. This entry point allows transports to be
      decorated without exposing their implementation. *)

  val close : t -> unit
  (** Close a transport exactly once. After closure, new requests fail without
      invoking the callback. *)
end

type api_error = {
  code : int;
  reason : string option;
  message : string;
      (** Server-requested delay from [Status.details.retryAfterSeconds] or an
          HTTP [Retry-After] delta-seconds header. *)
  retry_after_seconds : int option;
  body : Yojson.Safe.t option;
}

type error =
  | Transport of string
  | Api of api_error
  | Decode of string
  | Invalid_request of string

module Error : sig
  (** Kubernetes-aware error inspection. A known Status [reason] takes
      precedence over its HTTP code, so an [AlreadyExists] response is not also
      classified as an ordinary update conflict merely because both use 409. *)

  val status_code : error -> int option
  val reason : error -> string option
  val is_unauthorized : error -> bool
  val is_forbidden : error -> bool
  val is_not_found : error -> bool
  val is_already_exists : error -> bool
  val is_conflict : error -> bool
  val is_gone : error -> bool
  val is_resource_expired : error -> bool
  val is_invalid : error -> bool
  val is_bad_request : error -> bool
  val is_method_not_supported : error -> bool
  val is_not_acceptable : error -> bool
  val is_request_entity_too_large : error -> bool
  val is_unsupported_media_type : error -> bool
  val is_timeout : error -> bool
  val is_server_timeout : error -> bool
  val is_too_many_requests : error -> bool
  val is_internal_error : error -> bool
  val is_service_unavailable : error -> bool

  val suggested_delay : error -> float option
  (** Return the server-requested retry delay. [ServerTimeout] implies a delay
      even when Kubernetes supplied zero seconds. This does not by itself mean
      that blindly replaying the failed operation is safe. *)

  val is_transient : error -> bool
  (** Whether a fresh attempt may succeed after transport recovery or delay.
      Conflicts are deliberately excluded because callers generally must first
      fetch current state and recompute their write. *)
end

type patch =
  | Json_patch of Yojson.Safe.t
  | Merge_patch of Yojson.Safe.t
  | Apply of { value : Yojson.Safe.t; field_manager : string; force : bool }
      (** Kubernetes patch media types. [Apply] uses Server-Side Apply and
          therefore requires a stable field-manager identity. JSON is valid YAML
          and is sent with the apply media type. *)

type field_validation = [ `Ignore | `Warn | `Strict ]

type write_options = {
  dry_run : string list;
  field_manager : string option;
  field_validation : field_validation option;
}
(** Query options shared by CREATE, UPDATE, and PATCH. Kubernetes accepts
    ["All"] as the standard dry-run directive. *)

val default_write_options : write_options

type propagation_policy = [ `Orphan | `Background | `Foreground ]

type delete_options = {
  grace_period_seconds : int option;
  propagation_policy : propagation_policy option;
  precondition_uid : string option;
  precondition_resource_version : Core.Resource_version.t option;
  dry_run : string list;
}
(** Kubernetes [DeleteOptions], including optimistic preconditions and garbage
    collection policy. *)

val default_delete_options : delete_options

type scale_spec = { replicas : int32 }
type scale_status = { replicas : int32; selector : string option }

type scale = {
  api_version : string;
  kind : string;
  metadata : Core.object_meta;
  spec : scale_spec;
  status : scale_status option;
}
(** The generic [autoscaling/v1] Scale representation returned by scalable
    resource subresources. An omitted [spec.replicas] decodes as zero, matching
    the Kubernetes wire default. *)

val scale_of_json : Yojson.Safe.t -> (scale, string) result
val scale_to_json : scale -> Yojson.Safe.t

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
(** Kubernetes Pod log query options. [stream] is supported by API servers with
    split-stream log queries enabled. *)

val default_log_options : log_options

type 'a list_result = {
  items : 'a list;
  resource_version : Core.Resource_version.t;
  continue_token : string option;
  remaining_item_count : int option;
}
(** One page or a fully aggregated consistent collection snapshot. *)

type 'a watch_event =
  | Added of 'a
  | Modified of 'a
  | Deleted of 'a
  | Bookmark of Core.Resource_version.t option
  | Watch_error of api_error
      (** Decoded Kubernetes watch envelopes. Bookmark objects intentionally
          expose only their resource version. *)

type watch_outcome =
  | Watch_ended of Core.Resource_version.t
  | Resource_version_expired

val create :
  ?max_idle_connections:int ->
  ?connect_timeout:float ->
  ?write_timeout:float ->
  ?response_header_timeout:float ->
  ?rate_limiter:Rate_limiter.t ->
  ?logger:Log.t ->
  Config.t ->
  t
(** Create a client with a thread-safe persistent HTTP connection pool. The
    default per-client limiter allows 20 requests/second with a burst of 30;
    pass [Rate_limiter.unlimited] or a shared custom limiter to override it. New
    connections have a 10-second DNS/TCP/TLS deadline by default. Request writes
    and response headers each have a 30-second deadline by default; response
    bodies remain unbounded for watch streams. A supplied logger is shared by
    client and controller-runtime components; the default logger is disabled. *)

val create_with_transport :
  ?rate_limiter:Rate_limiter.t ->
  ?logger:Log.t ->
  transport:Transport.t ->
  Config.t ->
  t
(** Create a client around an injected transport. Authentication, rate limiting,
    API-status classification, decoding, and request logging remain active. This
    is intended for deterministic tests, protocol record/replay, and specialized
    deployment transports. The client owns [transport]: closing this client
    closes it, so one transport must not be passed to independent clients. *)

val close : t -> unit
(** Close the owned transport. New requests fail after this call. For the native
    HTTP transport, idle connections close immediately and concurrent requests
    are allowed to finish. *)

val config : t -> Config.t
val logger : t -> Log.t
val pp_error : Format.formatter -> error -> unit

val raw :
  ?cancel:Cancel.t ->
  ?headers:(string * string) list ->
  ?body:string ->
  ?on_chunk:(string -> unit) ->
  ?max_body_bytes:int ->
  t ->
  Http.meth ->
  string ->
  (Http.response, error) result
(** Execute an authenticated request. Refreshable exec credentials are
    invalidated and retried once after a 401 response. *)

val websocket :
  ?cancel:Cancel.t ->
  ?headers:(string * string) list ->
  ?protocols:string list ->
  ?max_message_bytes:int ->
  ?max_error_body_bytes:int ->
  t ->
  string ->
  (Websocket.t, error) result
(** Open an authenticated WebSocket subresource connection through the owned
    transport. Rate limiting, impersonation, credential refresh after 401,
    Kubernetes Status decoding, and request logging match ordinary requests.
    Injected transports must opt into upgrades through [Transport.make]. *)

module For (Resource : Core.Resource) : sig
  (** Typed operations for one resource descriptor. Object operations default to
      the explicit namespace, then kubeconfig namespace, then [default]. A
      namespace-less LIST or WATCH continues to mean all namespaces. *)

  val get :
    ?cancel:Cancel.t ->
    ?resource_version:Core.Resource_version.t ->
    ?resource_version_match:[ `Exact | `Not_older_than ] ->
    t ->
    ?namespace:string ->
    string ->
    (Resource.t, error) result

  val create :
    ?cancel:Cancel.t ->
    ?options:write_options ->
    t ->
    ?namespace:string ->
    Resource.t ->
    (Resource.t, error) result

  val replace :
    ?cancel:Cancel.t ->
    ?options:write_options ->
    t ->
    ?namespace:string ->
    string ->
    Resource.t ->
    (Resource.t, error) result

  val delete :
    ?cancel:Cancel.t ->
    ?options:delete_options ->
    t ->
    ?namespace:string ->
    string ->
    (unit, error) result

  val delete_collection :
    ?cancel:Cancel.t ->
    ?options:delete_options ->
    ?namespace:string ->
    ?all_namespaces:bool ->
    ?label_selector:string ->
    ?field_selector:string ->
    ?resource_version:Core.Resource_version.t ->
    ?resource_version_match:[ `Exact | `Not_older_than ] ->
    ?limit:int ->
    ?continue:string ->
    ?timeout_seconds:int ->
    t ->
    (unit, error) result
  (** Delete every object selected by Kubernetes ListOptions. The
      [delete_options] body controls preconditions, propagation, grace period,
      and dry-run behavior. Namespaced resources default to the configured or
      [default] namespace; [all_namespaces=true] is an explicit opt-in to the
      cross-namespace collection endpoint. *)

  val patch :
    ?cancel:Cancel.t ->
    ?options:write_options ->
    t ->
    ?namespace:string ->
    string ->
    patch ->
    (Resource.t, error) result

  val patch_status :
    ?cancel:Cancel.t ->
    ?options:write_options ->
    t ->
    ?namespace:string ->
    string ->
    patch ->
    (Resource.t, error) result

  val replace_status :
    ?cancel:Cancel.t ->
    ?options:write_options ->
    t ->
    ?namespace:string ->
    string ->
    Resource.t ->
    (Resource.t, error) result

  val get_subresource :
    ?cancel:Cancel.t ->
    t ->
    ?namespace:string ->
    string ->
    string ->
    (Yojson.Safe.t, error) result

  val create_subresource :
    ?cancel:Cancel.t ->
    ?options:write_options ->
    t ->
    ?namespace:string ->
    name:string ->
    subresource:string ->
    Yojson.Safe.t ->
    (Yojson.Safe.t, error) result

  val replace_subresource :
    ?cancel:Cancel.t ->
    ?options:write_options ->
    t ->
    ?namespace:string ->
    name:string ->
    subresource:string ->
    Yojson.Safe.t ->
    (Yojson.Safe.t, error) result

  val patch_subresource :
    ?cancel:Cancel.t ->
    ?options:write_options ->
    t ->
    ?namespace:string ->
    name:string ->
    subresource:string ->
    patch ->
    (Yojson.Safe.t, error) result

  val delete_subresource :
    ?cancel:Cancel.t ->
    ?options:delete_options ->
    t ->
    ?namespace:string ->
    string ->
    string ->
    (unit, error) result

  val stream_subresource :
    ?cancel:Cancel.t ->
    ?query:(string * string) list ->
    ?max_error_body_bytes:int ->
    t ->
    ?namespace:string ->
    string ->
    string ->
    on_chunk:(string -> unit) ->
    (unit, error) result
  (** Stream an arbitrary successful subresource body without buffering it.
      Chunks have transport boundaries and are not necessarily complete lines.
      Error responses remain bounded by [max_error_body_bytes]. *)

  val get_scale :
    ?cancel:Cancel.t ->
    t ->
    ?namespace:string ->
    string ->
    (scale, error) result

  val replace_scale :
    ?cancel:Cancel.t ->
    ?options:write_options ->
    t ->
    ?namespace:string ->
    string ->
    scale ->
    (scale, error) result

  val patch_scale :
    ?cancel:Cancel.t ->
    ?options:write_options ->
    t ->
    ?namespace:string ->
    string ->
    patch ->
    (scale, error) result

  val logs :
    ?cancel:Cancel.t ->
    ?options:log_options ->
    ?max_body_bytes:int ->
    t ->
    ?namespace:string ->
    string ->
    (string, error) result
  (** Read a Pod log response into memory, bounded by [max_body_bytes]. *)

  val stream_logs :
    ?cancel:Cancel.t ->
    ?options:log_options ->
    ?max_error_body_bytes:int ->
    t ->
    ?namespace:string ->
    string ->
    on_chunk:(string -> unit) ->
    (unit, error) result
  (** Stream Pod logs. Set [options.follow] for a continuing stream and cancel
      the supplied token to stop it. *)

  val list :
    ?cancel:Cancel.t ->
    ?namespace:string ->
    ?label_selector:string ->
    ?field_selector:string ->
    ?resource_version:Core.Resource_version.t ->
    ?resource_version_match:[ `Exact | `Not_older_than ] ->
    ?limit:int ->
    ?continue:string ->
    t ->
    (Resource.t list_result, error) result

  val list_all :
    ?cancel:Cancel.t ->
    ?namespace:string ->
    ?label_selector:string ->
    ?field_selector:string ->
    ?resource_version:Core.Resource_version.t ->
    ?resource_version_match:[ `Exact | `Not_older_than ] ->
    ?page_size:int ->
    t ->
    (Resource.t list_result, error) result
  (** Consume every continuation page from one consistent list snapshot. A
      failed continuation returns an error; callers must restart the whole list
      rather than combine inconsistent snapshots. *)

  val watch :
    ?cancel:Cancel.t ->
    ?namespace:string ->
    ?label_selector:string ->
    ?field_selector:string ->
    ?timeout_seconds:int ->
    ?allow_bookmarks:bool ->
    ?resource_version_match:[ `Exact | `Not_older_than ] ->
    ?send_initial_events:bool ->
    ?max_event_bytes:int ->
    t ->
    resource_version:Core.Resource_version.t ->
    on_event:(Resource.t watch_event -> unit) ->
    (watch_outcome, error) result
  (** Stream events from [resource_version]. A normal timeout/EOF returns the
      newest observed version. HTTP or in-band 410 is reported separately so a
      reflector can clear and rebuild its cache. Other in-band Status errors are
      delivered to [on_event] and returned as [Api] errors, allowing reconnect
      logic to apply backoff. Individual events are bounded by
      [max_event_bytes]. *)
end
