(** Kubernetes API group and resource discovery. *)

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

val server_version :
  ?cancel:Cancel.t -> Client.t -> (Yojson.Safe.t, Client.error) result

val core_versions :
  ?cancel:Cancel.t -> Client.t -> (string list, Client.error) result

val groups : ?cancel:Cancel.t -> Client.t -> (group list, Client.error) result

val resources :
  ?cancel:Cancel.t ->
  Client.t ->
  group:string ->
  version:string ->
  (resource_list, Client.error) result

module Mapper : sig
  (** Thread-safe, lazy discovery cache and exact REST mapping. Exact GVK/GVR
      lookups fetch only their requested group-version, so an unavailable
      aggregated API does not prevent discovery of unrelated resources. *)

  type subresource = { name : string; kind : string; verbs : string list }

  type mapping = {
    api : Core.api;
    singular_name : string;
    verbs : string list;
    short_names : string list;
    categories : string list;
    subresources : subresource list;
  }

  type t

  val create : Client.t -> t
  val client : t -> Client.t

  val invalidate : t -> unit
  (** Atomically discard cached discovery. In-flight requests may finish for
      their caller but cannot repopulate the invalidated generation. *)

  val resolve_gvk :
    ?cancel:Cancel.t ->
    t ->
    group:string ->
    version:string ->
    kind:string ->
    (mapping, Client.error) result
  (** Resolve one exact group/version/kind to its resource URL identity. *)

  val resolve_gvr :
    ?cancel:Cancel.t ->
    t ->
    group:string ->
    version:string ->
    resource:string ->
    (mapping, Client.error) result
  (** Resolve one exact group/version/resource. [resource] is the plural base
      resource name, not a subresource path. *)

  val preferred_version :
    ?cancel:Cancel.t -> t -> group:string -> (string, Client.error) result
  (** Return the preferred version advertised for one API group. The empty group
      denotes the core API, whose first advertised version is preferred. *)

  val resolve_kind :
    ?cancel:Cancel.t ->
    ?group:string ->
    t ->
    kind:string ->
    (mapping, Client.error) result
  (** Resolve a Kind in the preferred version of one group. With no [group],
      search every preferred group-version and reject ambiguous Kinds. A
      discovery failure in any searched group fails an unqualified lookup. *)

  val resolve_resource :
    ?cancel:Cancel.t ->
    ?group:string ->
    t ->
    resource:string ->
    (mapping, Client.error) result
  (** Resolve a plural resource name, singular name, or server-advertised short
      name in preferred group versions. With no [group], ambiguity is an error
      and every preferred group must be discoverable. *)

  val mappings : ?cancel:Cancel.t -> t -> (mapping list, Client.error) result
  (** Discover every advertised core and named group-version and return all
      base-resource mappings in stable API order. Unlike exact resolution, a
      failure in any advertised group-version fails the complete operation. *)

  val refresh : ?cancel:Cancel.t -> t -> (mapping list, Client.error) result
  (** Invalidate the cache and perform complete discovery. *)
end
