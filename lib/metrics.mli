(** Minimal thread-safe Prometheus text-format metrics. Metric families are
    registered explicitly and each handle represents one fixed label set. *)

type t
type registry = t

val create : unit -> t
val render : t -> string
val content_type : string

module Counter : sig
  type t

  val create :
    registry:registry ->
    name:string ->
    help:string ->
    ?labels:(string * string) list ->
    unit ->
    t

  val add : t -> float -> unit
  val inc : t -> unit
  val value : t -> float
end

module Gauge : sig
  type t

  val create :
    registry:registry ->
    name:string ->
    help:string ->
    ?labels:(string * string) list ->
    unit ->
    t

  val set : t -> float -> unit
  val add : t -> float -> unit
  val inc : t -> unit
  val dec : t -> unit
  val value : t -> float
end

module Histogram : sig
  type t

  val create :
    registry:registry ->
    name:string ->
    help:string ->
    buckets:float list ->
    ?labels:(string * string) list ->
    unit ->
    t
  (** Buckets must be finite, strictly increasing upper bounds. [+Inf] is
      emitted automatically. *)

  val observe : t -> float -> unit
end
