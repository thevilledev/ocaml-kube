(** Kubernetes Pod exec and attach over versioned channel WebSockets. *)

type t
type protocol = V1 | V2 | V3 | V4 | V5
type stream = Stdin | Stdout | Stderr | Error_stream | Resize
type exit_status = Success | Exit_code of int

type remote_status = {
  status : string option;
  reason : string option;
  message : string;
  code : int option;
  body : Yojson.Safe.t option;
}

type event =
  | Stdout_data of string
  | Stderr_data of string
  | Stream_closed of stream
  | Exit of exit_status
  | Remote_error of remote_status
  | Connection_closed of Websocket.close

type error = Client_error of Client.error | Protocol_error of string

val pp_error : Format.formatter -> error -> unit
val protocol : t -> protocol

val exec :
  ?cancel:Cancel.t ->
  ?namespace:string ->
  ?container:string ->
  ?stdin:bool ->
  ?stdout:bool ->
  ?stderr:bool ->
  ?tty:bool ->
  ?protocols:protocol list ->
  Client.t ->
  pod:string ->
  command:string list ->
  unit ->
  (t, error) result
(** Open an exec session. The default streams are stdout and stderr, with no
    stdin or TTY. WebSocket protocol V5 is requested by default, matching
    current Kubernetes clients and preserving stdin half-close semantics. *)

val attach :
  ?cancel:Cancel.t ->
  ?namespace:string ->
  ?container:string ->
  ?stdin:bool ->
  ?stdout:bool ->
  ?stderr:bool ->
  ?tty:bool ->
  ?protocols:protocol list ->
  Client.t ->
  pod:string ->
  unit ->
  (t, error) result
(** Open an attach session using the same stream protocol as [exec]. *)

val send_stdin : t -> string -> (unit, error) result
(** Send one stdin chunk. Concurrent calls are serialized by the WebSocket
    transport. *)

val close_stdin : t -> (unit, error) result
(** Half-close stdin using the V5 CLOSE signal. Idempotent. Older negotiated
    protocols cannot represent this operation and return [Protocol_error]. *)

val resize : t -> width:int -> height:int -> (unit, error) result
(** Send a terminal-size update. Requires a TTY and protocol V3 or newer. *)

val receive : t -> (event, error) result
(** Receive one complete stream event. Only one receiver runs at a time; stdin,
    resize, and close writes may proceed concurrently. *)

val close : t -> unit
(** Close the complete session exactly once. *)
