(** Native HTTP server for Kubernetes health probes and Prometheus scraping. The
    default bind address is loopback; callers must explicitly select a
    pod-visible address such as [0.0.0.0]. *)

type t

val create :
  ?address:string ->
  ?port:int ->
  ?health:Health.t ->
  ?metrics:Metrics.t ->
  unit ->
  t

val health : t -> Health.t
val metrics : t -> Metrics.t
val component : t -> Manager.component
val bound_port : t -> int option

val await_listening : cancel:Cancel.t -> t -> (int, string) result
(** Wait for bind completion. This is primarily useful for tests and port-zero
    embedding; normal applications can register [component] with a manager. *)
