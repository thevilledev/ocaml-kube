(** Kubernetes Pod port forwarding over the WebSocket SPDY tunnel. *)

type t
type stream

type error =
  | Client_error of Client.error
  | Protocol_error of string
  | Io_error of string

val pp_error : Format.formatter -> error -> unit

val connect :
  ?cancel:Cancel.t ->
  ?namespace:string ->
  ?ports:int list ->
  ?max_stream_buffer_bytes:int ->
  Client.t ->
  pod:string ->
  unit ->
  (t, error) result
(** Open one multiplexed port-forward connection. [ports] is sent as repeated
    query parameters and should contain the remote ports that will be opened.
    Current Kubernetes API servers require the [SPDY/3.1+portforward.k8s.io]
    WebSocket subprotocol. *)

val open_stream : ?timeout:float -> t -> port:int -> (stream, error) result
(** Open the error/data stream pair for one connection to [port]. *)

val read : stream -> bytes -> int -> int -> (int, error) result
val write : stream -> string -> (unit, error) result

val close_write : stream -> (unit, error) result
(** Half-close the data stream after all local input has been sent. *)

val close_stream : stream -> unit
(** Abort both streams. Idempotent. *)

val close : t -> unit
(** Close the multiplexed connection and all of its streams. Idempotent. *)

module Forwarder : sig
  type connection = t
  type t
  type mapping = { local_port : int; remote_port : int }
  type bound_port = { address : string; local_port : int; remote_port : int }

  val start :
    ?cancel:Cancel.t ->
    ?namespace:string ->
    ?addresses:string list ->
    ?max_stream_buffer_bytes:int ->
    ?on_connection_error:(error -> unit) ->
    Client.t ->
    pod:string ->
    ports:mapping list ->
    unit ->
    (t, error) result
  (** Bind local TCP listeners and forward every accepted connection through a
      shared Kubernetes port-forward tunnel. Addresses must be IP literals; the
      default is 127.0.0.1. A local port of zero asks the OS to choose a free
      port. *)

  val bound_ports : t -> bound_port list
  val connection : t -> connection

  val await : t -> (unit, error) result
  (** Wait until cancellation, explicit closure, or a fatal tunnel/listener
      failure. *)

  val close : t -> unit
  (** Stop accepting, close active local connections, and wait for forwarding
      workers to finish. *)
end

module For_testing : sig
  type peer

  val peer : unit -> peer

  val decode_syn_stream :
    peer -> string -> (int * (string * string) list, string) result

  val syn_reply : peer -> stream_id:int -> string
  val data : stream_id:int -> fin:bool -> string -> string
  val decode_data : string -> (int * bool * string, string) result
end
