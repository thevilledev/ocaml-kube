(** Reflector-backed reconciliation controllers. *)

type reconcile_result = Done | Requeue | Requeue_after of float

module Make (Resource : Core.Resource) : sig
  type request = { key : Core.Object_key.t; resource : Resource.t option }

  val run :
    ?cancel:Cancel.t ->
    ?namespace:string ->
    ?label_selector:string ->
    ?workers:int ->
    Client.t ->
    reconcile:(Client.t -> request -> (reconcile_result, string) result) ->
    (unit, Client.error) result
  (** LIST the initial state, WATCH from its resource version, and run bounded
      concurrent reconcilers until cancellation. A 410 response atomically
      resets the local store from a new LIST. *)
end

module Finalizer (Resource : Core.Resource) : sig
  val ensure :
    ?cancel:Cancel.t ->
    Client.t ->
    Resource.t ->
    string ->
    (Resource.t, Client.error) result
  (** Optimistic finalizer mutation helpers. Conflicts are returned to the
      reconciler so its work-queue retry policy can re-read and try again. *)

  val remove :
    ?cancel:Cancel.t ->
    Client.t ->
    Resource.t ->
    string ->
    (Resource.t, Client.error) result
end
