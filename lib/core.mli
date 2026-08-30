(** Kubernetes API machinery types shared by typed and dynamic resources. *)

type scope = Namespaced | Cluster

module Resource_version : sig
  (** Opaque etcd-backed version identifier. Values are only suitable for
      round-tripping to the API server; they are not client-side counters. *)

  type t

  val of_string : string -> t
  val to_string : t -> string
end

module Object_key : sig
  (** Stable queue/cache identity: optional namespace plus object name. *)

  type t = { namespace : string option; name : string }

  val make : ?namespace:string -> string -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val to_string : t -> string
end

type owner_reference = {
  api_version : string;
  kind : string;
  name : string;
  uid : string;
  controller : bool;
  block_owner_deletion : bool;
}

type object_reference = {
  api_version : string;
  kind : string;
  namespace : string option;
  name : string;
  uid : string option;
  resource_version : Resource_version.t option;
  field_path : string option;
}
(** A Kubernetes [ObjectReference], used by Events and other cross-object API
    fields. Unlike an owner reference, its UID is optional on the wire. *)

type object_meta = {
  name : string;
  namespace : string option;
  uid : string option;
  resource_version : Resource_version.t option;
  generation : int option;
  deletion_timestamp : string option;
  finalizers : string list;
  owner_references : owner_reference list;
  labels : (string * string) list;
  annotations : (string * string) list;
}

type api = {
  group : string;
  version : string;
  kind : string;
  plural : string;
  scope : scope;
}

module type Resource = sig
  (** A typed resource module. Generated resources and hand-written CRDs
      implement this signature. *)

  type t

  val api : api
  val metadata : t -> object_meta
  val of_json : Yojson.Safe.t -> (t, string) result
  val to_json : t -> Yojson.Safe.t
end

val api_version : api -> string

val collection_path : api -> namespace:string option -> (string, string) result
(** Build a collection endpoint. For a namespaced resource, [None] denotes the
    all-namespaces collection and is therefore appropriate for LIST/WATCH. *)

val object_path :
  api -> namespace:string option -> name:string -> (string, string) result

val subresource_path :
  api ->
  namespace:string option ->
  name:string ->
  subresource:string ->
  (string, string) result

val object_meta_of_json : Yojson.Safe.t -> (object_meta, string) result

val object_reference :
  ?field_path:string -> api -> object_meta -> object_reference

val object_reference_to_json : object_reference -> Yojson.Safe.t

val make_owner_reference :
  ?controller:bool ->
  ?block_owner_deletion:bool ->
  api ->
  object_meta ->
  (owner_reference, string) result
(** Construct an owner reference from authoritative object metadata. A UID is
    required because Kubernetes owner references without one are invalid. *)

val controller_owner_reference :
  api -> object_meta -> (owner_reference, string) result
(** Construct the conventional controlling, deletion-blocking owner reference.
*)

val owner_reference_to_json : owner_reference -> Yojson.Safe.t
val object_meta_to_json : object_meta -> Yojson.Safe.t
val key_of_meta : object_meta -> Object_key.t
