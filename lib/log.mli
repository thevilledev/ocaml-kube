(** Thread-safe structured logging for client and controller components. *)

type level = Debug | Info | Warn | Error

type value =
  | String of string
  | Int of int
  | Int64 of int64
  | Float of float
  | Bool of bool
  | Redacted

type field = string * value

type event = {
  timestamp : Ptime.t;
  level : level;
  message : string;
  fields : field list;
}

type sink = event -> unit
type t

val create :
  ?min_level:level -> ?now:(unit -> Ptime.t) -> sink:sink -> unit -> t
(** Create a logger. Sink calls are serialized across all derived loggers. Sink
    exceptions are isolated and counted instead of escaping into client or
    controller work. A sink must not recursively call the same logger. *)

val null : t
(** A disabled logger used by default. *)

val stderr : ?min_level:level -> unit -> t
(** Emit one JSON object per line to standard error. *)

val with_fields : t -> field list -> t
(** Derive a logger with immutable contextual fields. More local fields replace
    context fields of the same name. *)

val with_name : t -> string -> t
(** Add or replace the [logger] field. *)

val min_level : t -> level

val set_min_level : t -> level -> unit
(** Change the shared minimum level for this logger and all derived loggers. *)

val enabled : t -> level -> bool
val dropped_events : t -> int
val log : t -> level -> ?fields:field list -> string -> unit
val debug : t -> ?fields:field list -> string -> unit
val info : t -> ?fields:field list -> string -> unit
val warn : t -> ?fields:field list -> string -> unit
val error : t -> ?fields:field list -> string -> unit
val level_to_string : level -> string
val event_to_yojson : event -> Yojson.Safe.t
