(** Blocking, cancellation-aware HTTP/1.1 transport for the Kubernetes API.
    Successful streaming responses invoke [on_chunk] without retaining the body.
    Non-streaming and error responses are bounded by [max_body_bytes]. The
    transport owns Host, Content-Length, Transfer-Encoding, and Connection. *)

type meth = [ `GET | `POST | `PUT | `PATCH | `DELETE ]

type response = {
  status : int;
  reason : string;
  headers : (string * string) list;
  body : string;
}

type t

module Upgrade : sig
  type t

  val read : t -> bytes -> int -> int -> (int, string) result
  (** Read bytes from the upgraded connection. Bytes received in the same packet
      as the HTTP response headers are returned first. At most one reader may be
      active; concurrent writes are supported. [Ok 0] means EOF. *)

  val write : t -> string -> (unit, string) result
  (** Write all bytes, serializing concurrent writers. *)

  val is_closed : t -> bool

  val close : t -> unit
  (** Shut down and close the connection exactly once. *)
end

type upgrade_result =
  | Upgraded of { response : response; connection : Upgrade.t }
  | Response of response

val create :
  ?max_idle_connections:int ->
  ?connect_timeout:float ->
  ?write_timeout:float ->
  ?response_header_timeout:float ->
  Config.t ->
  t
(** Create a thread-safe transport. Fully consumed, self-delimiting HTTP/1.1
    responses are kept for reuse, up to [max_idle_connections] (default 8). DNS
    lookup, TCP connect, and the TLS handshake share a bounded monotonic
    [connect_timeout] (default 10 seconds). Writing the request and waiting for
    the response headers have independent deadlines (both default 30 seconds).
    Response bodies have no transport deadline because Kubernetes watch streams
    are intentionally long-lived; cancel the request to stop them. *)

val close : t -> unit
(** Close every idle connection and reject future requests. Concurrent requests
    already in progress are allowed to finish and are not returned to the pool.
*)

val request :
  ?cancel:Cancel.t ->
  ?headers:(string * string) list ->
  ?body:string ->
  ?on_chunk:(string -> unit) ->
  ?max_body_bytes:int ->
  t ->
  meth ->
  string ->
  (response, string) result
(** Execute a request using the transport's connection pool. A connection is
    discarded after cancellation, framing or callback errors, EOF-delimited
    responses, or a server [Connection: close] directive. *)

val request_once :
  ?cancel:Cancel.t ->
  ?headers:(string * string) list ->
  ?body:string ->
  ?on_chunk:(string -> unit) ->
  ?max_body_bytes:int ->
  ?connect_timeout:float ->
  ?write_timeout:float ->
  ?response_header_timeout:float ->
  Config.t ->
  meth ->
  string ->
  (response, string) result
(** Execute one request on a connection that is always closed afterwards. The
    timeout defaults and phase semantics are the same as [create]. *)

val upgrade_once :
  ?cancel:Cancel.t ->
  ?headers:(string * string) list ->
  ?max_body_bytes:int ->
  ?connect_timeout:float ->
  ?write_timeout:float ->
  ?response_header_timeout:float ->
  Config.t ->
  string ->
  (upgrade_result, string) result
(** Open one HTTP/1.1 Upgrade connection. The caller supplies [Upgrade] and any
    subprotocol headers; the transport owns HTTP framing and [Connection]. A
    non-101 response is fully consumed into a bounded body and returned as
    [Response]. A successful [Upgraded] connection owns the socket until
    explicitly closed or the request cancellation token fires. *)
