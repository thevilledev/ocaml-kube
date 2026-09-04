(** Kubernetes-aware retry helpers for optimistic writes. *)

type backoff

val exponential :
  ?initial:float ->
  ?maximum:float ->
  ?factor:float ->
  ?jitter:float ->
  max_attempts:int ->
  unit ->
  backoff
(** Build a retry schedule. [max_attempts] includes the first invocation. Delays
    and factors must be finite and non-negative; [factor] must be at least one
    and [jitter] is the maximum proportional delay added to each attempt. *)

val default_conflict : backoff
(** A short five-attempt schedule suitable for two clients contending for the
    same resource, analogous to client-go's default conflict retry. *)

val on_error :
  ?cancel:Cancel.t ->
  ?backoff:backoff ->
  retry:(Client.error -> bool) ->
  (unit -> ('a, Client.error) result) ->
  ('a, Client.error) result
(** Retry an operation while [retry] accepts its error. API-server [Retry-After]
    hints are treated as lower bounds. Cancellation interrupts the wait and
    returns the most recent error unchanged. *)

val on_conflict :
  ?cancel:Cancel.t ->
  ?backoff:backoff ->
  (unit -> ('a, Client.error) result) ->
  ('a, Client.error) result
(** Retry only Kubernetes update conflicts. The callback must perform a fresh
    GET and recompute its write on every invocation. *)
