(** Supervised HTTPS server for Kubernetes admission webhooks. Connections,
    request headers, and request bodies are bounded; every connection has a
    cancellation-aware monotonic deadline. *)

type t

type handler =
  cancel:Cancel.t -> Yojson.Safe.t -> (Yojson.Safe.t, string) result
(** A JSON review protocol. [Error] means the incoming review was malformed and
    becomes HTTP 400; internal application failures should be represented in a
    protocol-valid response. Uncaught exceptions become HTTP 500. *)

val create :
  ?address:string ->
  ?port:int ->
  ?max_connections:int ->
  ?max_header_bytes:int ->
  ?max_body_bytes:int ->
  ?request_timeout:float ->
  ?client_ca_pem:string ->
  ?metrics:Metrics.t ->
  certificate_pem:string ->
  private_key_pem:string ->
  unit ->
  (t, string) result
(** Create a TLS 1.2/1.3 server. [client_ca_pem], when supplied, requires and
    authenticates client certificates. The default bind address is loopback;
    production deployments normally select [0.0.0.0]. *)

val add : t -> path:string -> handler -> unit
(** Register one exact path. Paths must begin with [/] and cannot contain a
    query or fragment. Registration is rejected after the server starts. *)

val add_admission : t -> path:string -> Admission.handler -> unit
(** Register an [admission.k8s.io/v1] handler. Handler failures are converted to
    UID-preserving denied AdmissionResponses by [Admission.respond]. *)

val add_conversion : t -> path:string -> Conversion.handler -> unit
(** Register an [apiextensions.k8s.io/v1] CRD conversion handler. Conversion
    failures become UID-preserving failure ConversionResponses. *)

val component : t -> Manager.component
val bound_port : t -> int option

val await_listening : cancel:Cancel.t -> t -> (int, string) result
(** Wait for bind completion. Useful for port-zero tests and embedding. *)

val readiness_check : t -> unit -> (unit, string) result
(** A [Health.add_readiness]-compatible check that succeeds only while the
    server is accepting connections. *)
