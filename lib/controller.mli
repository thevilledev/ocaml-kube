(** Reflector-backed reconciliation controllers. *)

type reconcile_result = Done | Requeue | Requeue_after of float
type error_action = Retry_after of float | Drop

type error_policy =
  key:Core.Object_key.t -> attempt:int -> error:string -> error_action

val exponential_backoff :
  ?initial:float -> ?maximum:float -> ?max_retries:int -> unit -> error_policy
(** Build a zero-based per-key exponential error policy. [max_retries] limits
    scheduled retries; exhausted keys are dropped until a later watch event
    marks them dirty again. Durations must be finite and non-negative. *)

val default_error_policy : error_policy

module Make (Resource : Core.Resource) : sig
  module Cache : module type of Reflector.Make (Resource)
  module Cached : module type of Cached_client.Make (Resource)

  type request = {
    key : Core.Object_key.t;
    resource : Resource.t option;
    reader : Cached.t;
    cancel : Cancel.t;
  }
  (* One level-triggered reconciliation request. [cancel] is the controller's
     structured lifetime token and must be passed to blocking client calls so
     shutdown can interrupt in-flight reconciliation. [reader] provides
     synchronized cached reads for the primary resource while keeping fresh
     reads and all writes explicitly live. *)

  type source

  module Watches (Watched : Core.Resource) : sig
    module Cache : module type of Reflector.Make (Watched)

    val make :
      ?cache:Cache.t ->
      ?namespace:string ->
      ?namespaces:string list ->
      ?label_selector:string ->
      ?field_selector:string ->
      ?predicate:(Cache.event -> bool) ->
      map:(Watched.t -> Core.Object_key.t list) ->
      unit ->
      source
    (** Watch another resource and map each accepted event to primary-resource
        reconciliation keys. Modified events map both their previous and current
        cached values, so relationship changes reconcile both sides. Duplicate
        keys are removed before enqueueing. A supplied cache may be shared
        across controllers. [namespaces] selects independent per-namespace
        streams and is mutually exclusive with [namespace]. *)
  end

  module Owns (Owned : Core.Resource) : sig
    module Cache : module type of Reflector.Make (Owned)

    val make :
      ?cache:Cache.t ->
      ?namespace:string ->
      ?namespaces:string list ->
      ?label_selector:string ->
      ?field_selector:string ->
      ?predicate:(Cache.event -> bool) ->
      unit ->
      source
    (** Reconcile the matching controller owner reference for owned-resource
        events. An ownership transfer reconciles both the previous and current
        owner. Namespaced owners inherit the dependent object's namespace.
        [namespaces] selects independent per-namespace streams and is mutually
        exclusive with [namespace]. *)
  end

  val component :
    ?cache:Cache.t ->
    ?namespace:string ->
    ?namespaces:string list ->
    ?label_selector:string ->
    ?field_selector:string ->
    ?watches:source list ->
    ?predicate:(Cache.event -> bool) ->
    ?name:string ->
    ?workers:int ->
    ?metrics:Metrics.t ->
    ?health:Health.t ->
    ?cache_sync_timeout:float ->
    ?reconcile_timeout:float ->
    ?error_policy:error_policy ->
    reconcile:(Client.t -> request -> (reconcile_result, string) result) ->
    unit ->
    Manager.component
  (** Build a composable manager component. The primary cache and all watched
      caches synchronize before workers start, with a 120-second default
      [cache_sync_timeout]. [reconcile_timeout], when supplied, gives every
      invocation a linked child cancellation token and classifies expiry as a
      retryable reconciliation error. Timeouts are cooperative: reconcilers must
      pass the request token to blocking work. [namespaces] selects an explicit
      set of namespaced streams and is mutually exclusive with [namespace].
      [health] registers a readiness check named [<controller>/cache-sync]. It
      succeeds only after every cache has synchronized, initial keys are seeded,
      and all workers have started; cancellation and shutdown make it fail. *)

  val run :
    ?cancel:Cancel.t ->
    ?cache:Cache.t ->
    ?namespace:string ->
    ?namespaces:string list ->
    ?label_selector:string ->
    ?field_selector:string ->
    ?watches:source list ->
    ?predicate:(Cache.event -> bool) ->
    ?name:string ->
    ?workers:int ->
    ?metrics:Metrics.t ->
    ?health:Health.t ->
    ?cache_sync_timeout:float ->
    ?reconcile_timeout:float ->
    ?error_policy:error_policy ->
    Client.t ->
    reconcile:(Client.t -> request -> (reconcile_result, string) result) ->
    (unit, Client.error) result
  (** Standalone convenience wrapper around [component] and a one-controller
      manager. A 410 response atomically resets the shared local cache from a
      new LIST. *)
end

module Finalizer (Resource : Core.Resource) : sig
  type event = Apply of Resource.t | Cleanup of Resource.t

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

  val run :
    ?cancel:Cancel.t ->
    Client.t ->
    Resource.t ->
    string ->
    (event -> (reconcile_result, string) result) ->
    (reconcile_result, string) result
  (** Enforce the finalizer state machine around one reconciliation callback.
      New objects first receive the finalizer and requeue. Live finalized
      objects receive [Apply]. Deleting finalized objects receive [Cleanup], and
      the finalizer is removed only after cleanup returns [Done]. *)
end
