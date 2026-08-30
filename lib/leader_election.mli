(** High-availability leader election using [coordination.k8s.io/v1] Lease
    objects. Expiry decisions use local observation time and do not trust clocks
    embedded in remote Lease records. *)

type config = {
  namespace : string;
  name : string;
  identity : string;
  lease_duration : float;
  renew_deadline : float;
  retry_period : float;
  release_on_cancel : bool;
}

val default : namespace:string -> name:string -> identity:string -> config
(** The client-go-compatible timing defaults are a 15-second lease, 10-second
    renewal deadline, and 2-second retry period. Structured shutdown permits
    safe release on cancellation, which is enabled by default. *)

type phase = Waiting | Leading | Stopped
type 'a outcome = Cancelled_before_leadership | Finished of 'a

type error =
  | Invalid_config of string
  | Client_error of Client.error
  | Leadership_lost
  | Callback_failed of string

val pp_error : Format.formatter -> error -> unit

val run :
  ?cancel:Cancel.t ->
  ?on_phase:(phase -> unit) ->
  Client.t ->
  config ->
  (Cancel.t -> 'a) ->
  ('a outcome, error) result
(** Compete for the configured Lease. Once acquired, invoke the callback in a
    supervised thread with a leadership-scoped cancellation token. On external
    cancellation or lease loss, the callback is cancelled and joined before
    [run] returns. A configured release is attempted only after that join. *)
