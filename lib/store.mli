(** Thread-safe local resource stores used by reflectors and reconcilers. *)

module Make (Resource : Core.Resource) : sig
  type t

  val create : unit -> t
  val get : t -> Core.Object_key.t -> Resource.t option

  val add_index :
    t -> name:string -> (Resource.t -> string list) -> (unit, string) result
  (** Atomically install a named many-to-many secondary index over both the
      current snapshot and future updates. Duplicate values returned for one
      resource are deduplicated. Index names are unique within a store. *)

  val by_index : t -> name:string -> string -> (Resource.t list, string) result
  (** Return the current resources associated with one index value, ordered by
      object key. The lookup and resource snapshot are atomic. *)

  val upsert : t -> Resource.t -> Resource.t option
  (** Insert or update a resource and return its previous cached value. *)

  val remove : t -> Core.Object_key.t -> Resource.t option
  (** Remove a key and return its previous cached value. *)

  val replace_with_previous :
    t ->
    Resource.t list ->
    (Resource.t option * Resource.t) list * Resource.t list
  (** Atomically replace the snapshot, returning each input resource paired with
      its previous value when the key already existed, followed by resources no
      longer present. Input pair order is preserved. *)

  val replace_namespace_with_previous :
    t ->
    namespace:string ->
    Resource.t list ->
    ((Resource.t option * Resource.t) list * Resource.t list, string) result
  (** Atomically replace one namespace while preserving every object in other
      namespaces. All input objects must carry exactly [namespace], keys must be
      unique, and secondary indexes are updated in the same critical section.
      Cluster-scoped resources are rejected. This operation is intended for
      independent per-namespace LIST/WATCH streams. *)

  val replace : t -> Resource.t list -> Resource.t list
  (** Atomically replace the snapshot and return resources no longer present. *)

  val items : t -> Resource.t list
  val length : t -> int
end
