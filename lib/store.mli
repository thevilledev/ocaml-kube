(** Thread-safe local resource stores used by reflectors and reconcilers. *)

module Make (Resource : Core.Resource) : sig
  type t

  val create : unit -> t
  val get : t -> Core.Object_key.t -> Resource.t option
  val upsert : t -> Resource.t -> unit
  val remove : t -> Core.Object_key.t -> unit
  val replace : t -> Resource.t list -> Core.Object_key.t list
  val items : t -> Resource.t list
  val length : t -> int
end
