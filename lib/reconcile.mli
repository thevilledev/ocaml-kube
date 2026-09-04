(** Common idempotent write patterns for reconcilers. *)

module Make (Resource : Core.Resource) : sig
  module Api : module type of Client.For (Resource)

  val set_controller_reference :
    owner_api:Core.api ->
    owner:Core.object_meta ->
    Resource.t ->
    (Resource.t, string) result
  (** Return the resource with an authoritative controller owner reference. An
      existing reference to the same UID is replaced. A different controller
      owner, cross-namespace ownership, or a namespaced owner of a
      cluster-scoped object is rejected before making an API request. *)

  val apply :
    ?cancel:Cancel.t ->
    ?namespace:string ->
    ?force:bool ->
    ?dry_run:bool ->
    ?field_validation:Client.field_validation ->
    Client.t ->
    field_manager:string ->
    Resource.t ->
    (Resource.t, Client.error) result
  (** Create or converge a resource with Server-Side Apply. The desired object's
      metadata supplies its name and, unless overridden, namespace.
      [field_manager] is the stable ownership identity recorded by Kubernetes.
      Missing TypeMeta is filled from [Resource.api]; conflicting TypeMeta is
      rejected locally.
  *)

  val apply_owned :
    ?cancel:Cancel.t ->
    ?namespace:string ->
    ?force:bool ->
    ?dry_run:bool ->
    ?field_validation:Client.field_validation ->
    Client.t ->
    field_manager:string ->
    owner_api:Core.api ->
    owner:Core.object_meta ->
    Resource.t ->
    (Resource.t, Client.error) result
  (** Set a controller owner reference and apply the desired resource in one
      operation. This is the usual pattern for children managed by an operator.
  *)
end
