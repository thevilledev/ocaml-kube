(** Generate a buildable OCaml operator project from a Kubernetes v1 CRD. *)

type definition
type scope = Namespaced | Cluster

type diagnostic = { path : string; reason : string }
(** A schema location emitted as lossless dynamic JSON and the reason a more
    precise OCaml type was not safe. *)

val load_crd : ?version:string -> string -> (definition, string) result
(** Parse one JSON or restricted-YAML CRD document and select an explicitly
    requested version, or its unique storage version. *)

val default_project_name : definition -> string

val diagnostics : definition -> diagnostic list
(** Deterministic dynamic-fallback diagnostics for the selected spec/status
    schemas. Empty means every selected schema shape has a typed mapping. *)

val render :
  ?project_name:string -> definition -> ((string * string) list, string) result
(** Render relative output paths and their complete contents without writing. *)

val generate :
  ?project_name:string ->
  output:string ->
  definition ->
  (string list, string) result
(** Atomically create a new project directory. Existing output paths are never
    overwritten. The returned paths are relative to [output]. *)

val render_init :
  ?project_name:string ->
  ?version:string ->
  ?plural:string ->
  ?singular:string ->
  ?scope:scope ->
  group:string ->
  kind:string ->
  unit ->
  ((string * string) list, string) result
(** Render a type-first starter project. Its OCaml [Spec], [Status], and [Phase]
    definitions derive both JSON codecs and the structural CRD schema. *)

val generate_init :
  ?project_name:string ->
  ?version:string ->
  ?plural:string ->
  ?singular:string ->
  ?scope:scope ->
  output:string ->
  group:string ->
  kind:string ->
  unit ->
  (string list, string) result
(** Atomically create a type-first starter project. Existing output paths are
    never overwritten. *)
