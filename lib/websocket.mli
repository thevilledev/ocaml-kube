(** RFC 6455 client transport used by Kubernetes streaming subresources. *)

type t
type close = { code : int option; reason : string }
type message = Binary of string | Text of string | Close of close

type connect_error =
  | Transport of string
  | Http_response of Http.response
  | Protocol of string

val connect :
  ?cancel:Cancel.t ->
  ?headers:(string * string) list ->
  ?protocols:string list ->
  ?max_message_bytes:int ->
  ?max_error_body_bytes:int ->
  ?connect_timeout:float ->
  ?write_timeout:float ->
  ?response_header_timeout:float ->
  Config.t ->
  string ->
  (t, connect_error) result
(** Perform an authenticated-agnostic WebSocket handshake. Callers supply
    authorization and impersonation headers. Incoming messages are bounded by
    [max_message_bytes], including fragmented messages. *)

val protocol : t -> string option
val is_closed : t -> bool
val send_binary : t -> string -> (unit, string) result
val send_text : t -> string -> (unit, string) result
val send_ping : t -> string -> (unit, string) result

val receive : t -> (message, string) result
(** Receive one complete data message. Ping frames are answered automatically;
    pong frames are consumed internally. *)

val close : ?code:int -> ?reason:string -> t -> unit
(** Send one close frame and close the underlying connection. Idempotent. *)

module For_testing : sig
  val handshake_accept : string -> string
  (** Compute the RFC 6455 accept value for a base64 client key. This small
      helper exists so deterministic protocol servers can exercise the real
      client framing without duplicating the handshake algorithm. *)
end
