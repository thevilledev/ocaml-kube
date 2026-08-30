(** Cooperative cancellation shared by requests, reflectors, schedulers, and
    controllers. Cancellation is idempotent and safe across system threads. *)

type t

exception Cancelled

val create : unit -> t
(** Create a live cancellation token. *)

val is_cancelled : t -> bool

val check : t -> unit
(** Raise [Cancelled] if cancellation has been requested. *)

val on_cancel : t -> (unit -> unit) -> unit -> unit
(** [on_cancel t callback] registers [callback] and returns an idempotent
    unregister function. A late registration runs immediately. *)

val cancel : t -> unit
(** Request cancellation and run registered callbacks once. *)

val sleep : t -> float -> bool
(** [sleep t seconds] waits until the deadline or cancellation. It returns
    [true] when the deadline elapsed and [false] when cancellation won. *)
