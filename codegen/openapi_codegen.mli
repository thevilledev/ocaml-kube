(** Deterministic Kubernetes Swagger 2.0 to OCaml code generation. *)

type output = {
  implementation : string;
  interface : string;
  pruned_schema : string;
  definition_count : int;
}

val generate :
  schema:Yojson.Safe.t -> manifest:Yojson.Safe.t -> (output, string) result
(** Generate types, JSON codecs, constructors, and resource descriptors for the
    resources selected by [manifest]. The returned schema contains precisely the
    transitive definition closure required to reproduce the output. *)

val derive_stable_manifest :
  schema:Yojson.Safe.t ->
  manifest:Yojson.Safe.t ->
  (Yojson.Safe.t, string) result
(** Replace [manifest.resources] with every stable built-in resource for which
    the Swagger document exposes both LIST and WATCH operations. Resource type,
    plural name, API group/version module, and namespace scope are derived from
    the operations and their list response schemas. Other manifest metadata is
    retained. *)

val derive_stable_manifest_with_metadata :
  schema:Yojson.Safe.t ->
  kubernetes_version:string ->
  source:string ->
  sha256:string ->
  (Yojson.Safe.t, string) result
(** Derive a new manifest directly from pinned source metadata. This is the
    bootstrap entry point for adding a Kubernetes schema version without copying
    an older resource allowlist. *)

val load_json : string -> (Yojson.Safe.t, string) result
val write_file : string -> string -> (unit, string) result
val check_file : string -> string -> (unit, string) result
