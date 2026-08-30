(** Typed Kubernetes HTTP operations, list pagination, patches, and watches. *)

type t

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

val create : Config.t -> t
val config : t -> Config.t
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

  val patch_subresource :
    ?cancel:Cancel.t ->
    ?options:write_options ->
    t ->
    ?namespace:string ->
    name:string ->
    subresource:string ->
    patch ->
    (Yojson.Safe.t, error) result

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
      reflector can clear and rebuild its cache. Individual events are bounded
      by [max_event_bytes]. *)
end
