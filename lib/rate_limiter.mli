(** Cancellation-aware token-bucket request throttling. *)

type t

val create : qps:float -> burst:int -> t
(** Create a limiter initially containing [burst] tokens. [qps] must be finite
    and positive; [burst] must be positive. *)

val unlimited : t

val acquire : ?cancel:Cancel.t -> t -> bool
(** Wait for one token. Returns [true] after acquiring it and [false] when
    cancellation wins. Concurrent callers share the configured bucket. *)
