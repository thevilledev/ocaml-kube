(** Typed LIST/WATCH reflectors with thread-safe local stores. A reflector is a
    shareable manager dependency: multiple controllers may subscribe to one API
    stream and cache. *)

module Make (Resource : Core.Resource) : sig
  type event =
    | Added of Resource.t
    | Modified of { previous : Resource.t option; current : Resource.t }
    | Deleted of Resource.t

  type t

  val create :
    ?namespace:string ->
    ?namespaces:string list ->
    ?label_selector:string ->
    ?field_selector:string ->
    unit ->
    t
  (** Configure one all-namespace stream by default, one stream with
      [namespace], or independent streams for an explicit non-empty [namespaces]
      set. [namespace] and [namespaces] are mutually exclusive. Selected
      namespaces are invalid for cluster-scoped resources. A multi-namespace
      cache becomes ready only after every initial LIST has been installed, and
      each later relist replaces only its own namespace. *)

  val component : t -> Manager.component
  (** Return the stable manager component that owns this reflector. Registering
      it through several controllers still starts only one LIST/WATCH loop. *)

  val subscribe : t -> (event -> unit) -> unit -> unit
  (** Register an event listener and return an idempotent unsubscribe function.
      Listener calls are serialized across namespace streams, and exceptions are
      isolated from the reflector. *)

  val await_ready : cancel:Cancel.t -> t -> (unit, Client.error) result
  (** Wait until the initial consistent LIST has replaced the cache. *)

  val is_ready : t -> bool
  (** Whether an initial snapshot is installed and the reflector is still
      running. Temporary watch reconnects retain readiness; terminal failure or
      shutdown clears it. *)

  val get : t -> Core.Object_key.t -> Resource.t option

  val add_index :
    t -> name:string -> (Resource.t -> string list) -> (unit, string) result

  val by_index : t -> name:string -> string -> (Resource.t list, string) result
  val items : t -> Resource.t list
  val length : t -> int
end
