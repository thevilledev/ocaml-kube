(** Bounded SPDY/3.1 client multiplexer for Kubernetes port forwarding. *)

type t
type stream

val create : ?max_stream_buffer_bytes:int -> Websocket.t -> (t, string) result

val create_stream :
  ?timeout:float -> t -> (string * string) list -> (stream, string) result

val identifier : stream -> int
val read : stream -> bytes -> int -> int -> (int, string) result
val write : stream -> string -> (unit, string) result
val close_write : stream -> (unit, string) result
val reset : stream -> unit
val close : t -> unit

module For_testing : sig
  val dictionary_length : int
  val dictionary_adler32 : int32

  type peer

  val peer : unit -> peer

  val decode_syn_stream :
    peer -> string -> (int * (string * string) list, string) result

  val syn_reply : peer -> stream_id:int -> string
  val data : stream_id:int -> fin:bool -> string -> string
  val reset : stream_id:int -> status:int -> string
  val decode_data : string -> (int * bool * string, string) result
end
