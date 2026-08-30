(** Kubernetes [apiextensions.k8s.io/v1] CRD conversion webhook protocol. *)

type request = {
  uid : string;
  desired_api_version : string;
  objects : Yojson.Safe.t list;
}

type handler = cancel:Cancel.t -> request -> (Yojson.Safe.t list, string) result

val map :
  (cancel:Cancel.t ->
  desired_api_version:string ->
  Yojson.Safe.t ->
  (Yojson.Safe.t, string) result) ->
  handler
(** Lift a one-object converter across a ConversionRequest while preserving
    order and stopping at the first error. *)

val request_of_review_json : Yojson.Safe.t -> (request, string) result
val request_to_review_json : request -> Yojson.Safe.t

val respond :
  cancel:Cancel.t -> handler -> Yojson.Safe.t -> (Yojson.Safe.t, string) result
(** Decode a ConversionReview and return a UID-preserving response. Conversion
    failures and exceptions become protocol-valid failure responses. Successful
    output is checked for count, requested [apiVersion], and stable
    name/namespace/UID identity before it is returned. *)
