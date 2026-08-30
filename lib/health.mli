(** Thread-safe liveness and readiness check registry. *)

type t
type failure = { check : string; message : string }

val create : unit -> t

val add_liveness :
  t -> name:string -> (unit -> (unit, string) result) -> unit -> unit

val add_readiness :
  t -> name:string -> (unit -> (unit, string) result) -> unit -> unit
(** Register a named check and return an idempotent removal function. Duplicate
    names are rejected. Check exceptions become failures rather than escaping.
*)

val liveness : t -> (unit, failure list) result
val readiness : t -> (unit, failure list) result
val pp_failures : Format.formatter -> failure list -> unit
