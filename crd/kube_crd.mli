(** Typed custom-resource schemas, CRD manifests, and resource codecs. *)

module Schema : sig
  type t

  val string :
    ?format:string ->
    ?enum:string list ->
    ?min_length:int ->
    ?max_length:int ->
    ?pattern:string ->
    unit ->
    t

  val integer :
    ?format:[ `Int32 | `Int64 ] -> ?minimum:int -> ?maximum:int -> unit -> t

  val number : ?format:[ `Float | `Double ] -> unit -> t
  val boolean : unit -> t
  val array : ?min_items:int -> ?max_items:int -> ?unique_items:bool -> t -> t

  val object_ :
    ?required:string list ->
    ?additional_properties:t ->
    ?preserve_unknown_fields:bool ->
    (string * t) list ->
    t

  val map : t -> t
  val one_of : t list -> t
  val int_or_string : unit -> t
  val preserve_unknown : unit -> t

  val raw : Yojson.Safe.t -> t
  (** Escape hatch for OpenAPI extensions not yet represented by this module. *)

  val describe : string -> t -> t
  val with_default : Yojson.Safe.t -> t -> t
  val nullable : t -> t
  val to_json : t -> Yojson.Safe.t
  val validate : t -> (unit, string list) result
end

module Condition : sig
  (** Kubernetes-standard status conditions for custom resources. *)

  type status = True | False | Unknown

  type t = {
    type_ : string;
    status : status;
    observed_generation : int64 option;
    last_transition_time : string;
    reason : string;
    message : string;
  }

  val make :
    ?observed_generation:int64 ->
    ?last_transition_time:string ->
    type_:string ->
    status:status ->
    reason:string ->
    message:string ->
    unit ->
    t
  (** Construct a condition, defaulting [last_transition_time] to the current
      UTC time. Type and reason must be non-empty. *)

  val find : string -> t list -> t option
  val is_true : string -> t list -> bool

  val set : ?now:(unit -> string) -> t -> t list -> t list * bool
  (** Insert or replace a condition by type. The prior transition time is kept
      while status is unchanged; it is advanced when status changes. The boolean
      reports whether any serialized field changed. *)

  val remove : string -> t list -> t list * bool
  val of_json : Yojson.Safe.t -> (t, string) result
  val to_json : t -> Yojson.Safe.t
  val schema : Schema.t
end

module Custom_resource_definition : sig
  type scale = {
    spec_replicas_path : string;
    status_replicas_path : string;
    label_selector_path : string option;
  }

  type printer_column_type = [ `Integer | `Number | `String | `Boolean | `Date ]
  type printer_column
  type version
  type t

  val scale :
    ?label_selector_path:string ->
    spec_replicas_path:string ->
    status_replicas_path:string ->
    unit ->
    scale

  val printer_column :
    ?format:string ->
    ?description:string ->
    ?priority:int ->
    name:string ->
    type_:printer_column_type ->
    json_path:string ->
    unit ->
    printer_column

  val version :
    ?served:bool ->
    ?storage:bool ->
    ?status:bool ->
    ?scale:scale ->
    ?printer_columns:printer_column list ->
    name:string ->
    schema:Schema.t ->
    unit ->
    version

  val make :
    ?singular:string ->
    ?list_kind:string ->
    ?short_names:string list ->
    ?categories:string list ->
    group:string ->
    kind:string ->
    plural:string ->
    scope:Kube.Core.scope ->
    versions:version list ->
    unit ->
    (t, string list) result

  val make_exn :
    ?singular:string ->
    ?list_kind:string ->
    ?short_names:string list ->
    ?categories:string list ->
    group:string ->
    kind:string ->
    plural:string ->
    scope:Kube.Core.scope ->
    versions:version list ->
    unit ->
    t

  val to_json : t -> Yojson.Safe.t
  val to_yaml : t -> string
end

module Resource : sig
  module type Value = sig
    type t

    val schema : Schema.t
    val of_json : Yojson.Safe.t -> (t, string) result
    val to_json : t -> Yojson.Safe.t
  end

  module type Definition = sig
    module Spec : Value
    module Status : Value

    val group : string
    val version : string
    val kind : string
    val plural : string
    val singular : string
    val scope : Kube.Core.scope
    val short_names : string list
    val categories : string list
  end

  module Make (Definition : Definition) : sig
    type t = {
      api_version : string;
      kind : string;
      metadata : Kube.Core.object_meta;
      spec : Definition.Spec.t;
      status : Definition.Status.t option;
    }

    val api : Kube.Core.api
    val metadata : t -> Kube.Core.object_meta
    val of_json : Yojson.Safe.t -> (t, string) result
    val to_json : t -> Yojson.Safe.t

    val make :
      ?api_version:string ->
      ?kind:string ->
      ?status:Definition.Status.t ->
      metadata:Kube.Core.object_meta ->
      spec:Definition.Spec.t ->
      unit ->
      t

    val with_status : Definition.Status.t option -> t -> t
    val status_merge_patch : Definition.Status.t -> Kube.Client.patch
    val crd : Custom_resource_definition.t
  end
end
