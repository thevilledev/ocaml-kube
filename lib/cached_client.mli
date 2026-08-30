(** Typed cache-backed reads paired with live Kubernetes writes.

    A cached client never silently falls back to the API server: a miss in a
    synchronized cache is reported as Kubernetes [NotFound], and an
    unsynchronized or stopped cache is an error. This makes stale and filtered
    cache semantics explicit. All mutation and subresource operations go
    directly through the underlying {!Client.t}. *)

module Make (Resource : Core.Resource) : sig
  module Cache : module type of Reflector.Make (Resource)
  module Live : module type of Client.For (Resource)

  type t

  val make : client:Client.t -> cache:Cache.t -> t
  val client : t -> Client.t
  val cache : t -> Cache.t
  val await_ready : cancel:Cancel.t -> t -> (unit, Client.error) result

  val get :
    t -> ?namespace:string -> string -> (Resource.t, Client.error) result
  (** Read one object from the synchronized cache. Namespaced resources use the
      explicit namespace, kubeconfig namespace, or [default], in that order. *)

  val get_by_key : t -> Core.Object_key.t -> (Resource.t, Client.error) result

  val list : ?namespace:string -> t -> (Resource.t list, Client.error) result
  (** Return a key-ordered snapshot of the cache's existing filtered scope.
      [namespace] further narrows namespaced resources; it does not expand a
      namespace- or selector-filtered reflector. *)

  val by_index :
    t -> name:string -> string -> (Resource.t list, Client.error) result

  val fresh_get :
    ?cancel:Cancel.t ->
    ?resource_version:Core.Resource_version.t ->
    ?resource_version_match:[ `Exact | `Not_older_than ] ->
    t ->
    ?namespace:string ->
    string ->
    (Resource.t, Client.error) result

  val fresh_list_all :
    ?cancel:Cancel.t ->
    ?namespace:string ->
    ?label_selector:string ->
    ?field_selector:string ->
    ?resource_version:Core.Resource_version.t ->
    ?resource_version_match:[ `Exact | `Not_older_than ] ->
    ?page_size:int ->
    t ->
    (Resource.t Client.list_result, Client.error) result

  val create :
    ?cancel:Cancel.t ->
    ?options:Client.write_options ->
    t ->
    ?namespace:string ->
    Resource.t ->
    (Resource.t, Client.error) result

  val replace :
    ?cancel:Cancel.t ->
    ?options:Client.write_options ->
    t ->
    ?namespace:string ->
    string ->
    Resource.t ->
    (Resource.t, Client.error) result

  val delete :
    ?cancel:Cancel.t ->
    ?options:Client.delete_options ->
    t ->
    ?namespace:string ->
    string ->
    (unit, Client.error) result

  val delete_collection :
    ?cancel:Cancel.t ->
    ?options:Client.delete_options ->
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
    (unit, Client.error) result

  val patch :
    ?cancel:Cancel.t ->
    ?options:Client.write_options ->
    t ->
    ?namespace:string ->
    string ->
    Client.patch ->
    (Resource.t, Client.error) result

  val patch_status :
    ?cancel:Cancel.t ->
    ?options:Client.write_options ->
    t ->
    ?namespace:string ->
    string ->
    Client.patch ->
    (Resource.t, Client.error) result

  val replace_status :
    ?cancel:Cancel.t ->
    ?options:Client.write_options ->
    t ->
    ?namespace:string ->
    string ->
    Resource.t ->
    (Resource.t, Client.error) result

  val get_subresource :
    ?cancel:Cancel.t ->
    t ->
    ?namespace:string ->
    string ->
    string ->
    (Yojson.Safe.t, Client.error) result

  val create_subresource :
    ?cancel:Cancel.t ->
    ?options:Client.write_options ->
    t ->
    ?namespace:string ->
    name:string ->
    subresource:string ->
    Yojson.Safe.t ->
    (Yojson.Safe.t, Client.error) result

  val replace_subresource :
    ?cancel:Cancel.t ->
    ?options:Client.write_options ->
    t ->
    ?namespace:string ->
    name:string ->
    subresource:string ->
    Yojson.Safe.t ->
    (Yojson.Safe.t, Client.error) result

  val patch_subresource :
    ?cancel:Cancel.t ->
    ?options:Client.write_options ->
    t ->
    ?namespace:string ->
    name:string ->
    subresource:string ->
    Client.patch ->
    (Yojson.Safe.t, Client.error) result

  val delete_subresource :
    ?cancel:Cancel.t ->
    ?options:Client.delete_options ->
    t ->
    ?namespace:string ->
    string ->
    string ->
    (unit, Client.error) result

  val stream_subresource :
    ?cancel:Cancel.t ->
    ?query:(string * string) list ->
    ?max_error_body_bytes:int ->
    t ->
    ?namespace:string ->
    string ->
    string ->
    on_chunk:(string -> unit) ->
    (unit, Client.error) result

  val get_scale :
    ?cancel:Cancel.t ->
    t ->
    ?namespace:string ->
    string ->
    (Client.scale, Client.error) result

  val replace_scale :
    ?cancel:Cancel.t ->
    ?options:Client.write_options ->
    t ->
    ?namespace:string ->
    string ->
    Client.scale ->
    (Client.scale, Client.error) result

  val patch_scale :
    ?cancel:Cancel.t ->
    ?options:Client.write_options ->
    t ->
    ?namespace:string ->
    string ->
    Client.patch ->
    (Client.scale, Client.error) result

  val logs :
    ?cancel:Cancel.t ->
    ?options:Client.log_options ->
    ?max_body_bytes:int ->
    t ->
    ?namespace:string ->
    string ->
    (string, Client.error) result

  val stream_logs :
    ?cancel:Cancel.t ->
    ?options:Client.log_options ->
    ?max_error_body_bytes:int ->
    t ->
    ?namespace:string ->
    string ->
    on_chunk:(string -> unit) ->
    (unit, Client.error) result
end
