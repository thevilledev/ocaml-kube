(** Process-local monotonic time for measuring durations and deadlines. *)

val now : unit -> float
(** Seconds from an unspecified monotonic epoch. The value is unaffected by
    civil-clock corrections and is meaningful only for differences within this
    process. *)

val deadline : float -> float
(** [deadline seconds] returns a monotonic deadline [seconds] from now. Negative
    durations are treated as zero. *)

val remaining : float -> float
(** Seconds remaining until a monotonic deadline, clamped to zero. *)

val elapsed : float -> float
(** Seconds elapsed since a previous [now] value, clamped to zero. *)
