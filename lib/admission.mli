(** Kubernetes [admission.k8s.io/v1] request handling. The protocol layer is
    independent of the webhook transport so handlers can be tested without a
    socket or certificate. *)

type group_version_kind = { group : string; version : string; kind : string }

type group_version_resource = {
  group : string;
  version : string;
  resource : string;
}

type operation = Create | Update | Delete | Connect | Unknown of string

type user_info = {
  username : string;
  uid : string option;
  groups : string list;
  extra : (string * string list) list;
}

type request = {
  uid : string;
  kind : group_version_kind;
  resource : group_version_resource;
  subresource : string option;
  request_kind : group_version_kind option;
  request_resource : group_version_resource option;
  request_subresource : string option;
  name : string option;
  namespace : string option;
  operation : operation;
  user_info : user_info;
  object_ : Yojson.Safe.t option;
  old_object : Yojson.Safe.t option;
  dry_run : bool option;
  options : Yojson.Safe.t option;
}

type patch_operation =
  | Add of { path : string; value : Yojson.Safe.t }
  | Remove of { path : string }
  | Replace of { path : string; value : Yojson.Safe.t }
  | Move of { from : string; path : string }
  | Copy of { from : string; path : string }
  | Test of { path : string; value : Yojson.Safe.t }
      (** RFC 6902 operations. Admission mutation responses support JSON Patch.
      *)

type denial = { code : int; reason : string; message : string }
type decision = Allowed | Denied of denial | Patched of patch_operation list

type outcome = {
  decision : decision;
  warnings : string list;
  audit_annotations : (string * string) list;
}

val allow :
  ?warnings:string list ->
  ?audit_annotations:(string * string) list ->
  unit ->
  outcome

val deny :
  ?code:int ->
  ?reason:string ->
  ?warnings:string list ->
  ?audit_annotations:(string * string) list ->
  string ->
  outcome

val patch :
  ?warnings:string list ->
  ?audit_annotations:(string * string) list ->
  patch_operation list ->
  outcome

type handler = cancel:Cancel.t -> request -> (outcome, string) result

val request_of_review_json : Yojson.Safe.t -> (request, string) result
(** Decode and validate an [admission.k8s.io/v1] AdmissionReview request. *)

val request_to_review_json : request -> Yojson.Safe.t
(** Encode a request AdmissionReview. Primarily useful for tests, proxies, and
    webhook conformance tooling. *)

val respond :
  cancel:Cancel.t -> handler -> Yojson.Safe.t -> (Yojson.Safe.t, string) result
(** Decode a review, invoke the handler, and produce a response review with the
    same UID. Handler errors and exceptions become denied AdmissionResponses
    with code 500; malformed reviews are returned as [Error] because no trusted
    request UID is available. *)

module For (Resource : Core.Resource) : sig
  type typed_request = {
    admission : request;
    object_ : Resource.t option;
    old_object : Resource.t option;
  }

  val handler :
    (cancel:Cancel.t -> typed_request -> (outcome, string) result) -> handler
  (** Check the AdmissionRequest GVK and decode current and previous objects.
      GVK or object decoding failures become denied responses with code 400. *)
end
