(** Level-triggered, dirty-key-deduplicating controller work queues. A key is
    never returned to two workers concurrently. *)

type t

val create : unit -> t
val add : t -> Core.Object_key.t -> unit
val take : t -> Core.Object_key.t option
val task_done : t -> Core.Object_key.t -> unit
val close : t -> unit
val length : t -> int

module Scheduler : sig
  type queue = t
  type t

  val create : cancel:Cancel.t -> queue -> t
  val schedule : t -> after:float -> Core.Object_key.t -> unit
  val stop : t -> unit
end
