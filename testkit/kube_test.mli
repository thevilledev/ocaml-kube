(** Deterministic test support for clients and controllers.

    This library exercises the real {!Kube.Client} request pipeline while
    replacing only the final network exchange. It is intended for unit tests;
    integration tests should still use a real Kubernetes API server. *)

module Transport : sig
  type request = {
    meth : Kube.Http.meth;
    target : string;
    headers : (string * string) list;
    body : string option;
    streaming : bool;
    max_body_bytes : int option;
  }
  (** A captured request after authentication and before HTTP framing. *)

  type outcome

  val respond :
    ?status:int ->
    ?reason:string ->
    ?headers:(string * string) list ->
    string ->
    outcome
  (** Return one buffered response body. When the client requested streaming, a
      successful body is delivered as one callback chunk. *)

  val respond_json :
    ?status:int ->
    ?reason:string ->
    ?headers:(string * string) list ->
    Yojson.Safe.t ->
    outcome

  val stream :
    ?status:int ->
    ?reason:string ->
    ?headers:(string * string) list ->
    ?wait_for_cancel:bool ->
    string list ->
    outcome
  (** Deliver distinct chunks in order. [wait_for_cancel] keeps the request open
      after the chunks and is useful for reflectors and controller tests. It
      requires the client request to carry a cancellation token. *)

  val fail : string -> outcome
  (** Return a transport failure rather than an HTTP response. *)

  type t

  val create : (request -> outcome) -> t
  (** Create a concurrent handler transport. The handler may be called from
      several threads and is responsible for synchronizing its own mutable
      state. Requests are captured safely by the testkit. *)

  val scripted : outcome list -> t
  (** Create a FIFO transport. An extra request becomes a transport failure and
      is reported by [verify_complete]. *)

  val client_transport : t -> Kube.Client.Transport.t
  val requests : t -> request list
  val request_count : t -> int
  val remaining : t -> int option
  val is_closed : t -> bool

  val verify_complete : t -> (unit, string) result
  (** Check that a scripted transport consumed every outcome and received no
      extra requests. Handler transports are always complete. *)

  val header : request -> string -> string option
  (** Perform a case-insensitive lookup in captured request headers. *)
end

val config :
  ?server:Uri.t ->
  ?namespace:string ->
  ?credential:Kube.Config.credential ->
  unit ->
  Kube.Config.t
(** Build a network-independent client configuration. *)

val client :
  ?configuration:Kube.Config.t ->
  ?rate_limiter:Kube.Rate_limiter.t ->
  ?logger:Kube.Log.t ->
  Transport.t ->
  Kube.Client.t
(** Create a real client pipeline over a test transport. *)
