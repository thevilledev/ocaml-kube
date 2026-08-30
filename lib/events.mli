(** Best-effort Kubernetes Event recording through [events.k8s.io/v1]. Events
    are supplemental diagnostics and a failure to publish one should not be
    treated as proof that the reconciled operation failed. *)

type event_type = Normal | Warning
type t

val api : Core.api

val create :
  ?namespace:string ->
  ?now:(unit -> Ptime.t) ->
  client:Client.t ->
  reporting_controller:string ->
  reporting_instance:string ->
  unit ->
  (t, string) result
(** Configure a recorder identity. [reporting_controller] is the stable
    controller name; [reporting_instance] should identify this process or Pod.
    [namespace] is used for Events regarding cluster-scoped objects. *)

val record :
  ?cancel:Cancel.t ->
  ?related:Core.object_reference ->
  t ->
  regarding:Core.object_reference ->
  type_:event_type ->
  reason:string ->
  action:string ->
  note:string ->
  (unit, Client.error) result
(** Create one Event. Reason and action are required and limited to 128 bytes;
    notes are limited to the Kubernetes-recommended 1 KiB. The Event is placed
    alongside a namespaced regarding object, or in the recorder/default
    namespace for a cluster-scoped object. *)
