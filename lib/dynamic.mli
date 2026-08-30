(** Unstructured access to resources discovered or defined at runtime. The
    original JSON object is retained so unknown fields survive round trips. *)

type t = {
  api_version : string;
  kind : string;
  metadata : Core.object_meta;
  value : Yojson.Safe.t;
}

val of_json : Yojson.Safe.t -> (t, string) result

val to_json : t -> Yojson.Safe.t
(** Direct decoding requires [apiVersion] and [kind]. API operations
    additionally fill missing type metadata from their explicit [Core.api]
    descriptor, as Kubernetes may omit TypeMeta from objects nested in a LIST.
*)

val get :
  ?cancel:Cancel.t ->
  ?resource_version:Core.Resource_version.t ->
  ?resource_version_match:[ `Exact | `Not_older_than ] ->
  Client.t ->
  api:Core.api ->
  ?namespace:string ->
  string ->
  (t, Client.error) result

val create :
  ?cancel:Cancel.t ->
  ?options:Client.write_options ->
  Client.t ->
  api:Core.api ->
  ?namespace:string ->
  t ->
  (t, Client.error) result

val replace :
  ?cancel:Cancel.t ->
  ?options:Client.write_options ->
  Client.t ->
  api:Core.api ->
  ?namespace:string ->
  string ->
  t ->
  (t, Client.error) result

val delete :
  ?cancel:Cancel.t ->
  ?options:Client.delete_options ->
  Client.t ->
  api:Core.api ->
  ?namespace:string ->
  string ->
  (unit, Client.error) result

val delete_collection :
  ?cancel:Cancel.t ->
  ?options:Client.delete_options ->
  ?namespace:string ->
  ?all_namespaces:bool ->
  ?label_selector:string ->
  ?field_selector:string ->
  ?resource_version:Core.Resource_version.t ->
  ?resource_version_match:[ `Exact | `Not_older_than ] ->
  ?limit:int ->
  ?continue:string ->
  ?timeout_seconds:int ->
  Client.t ->
  api:Core.api ->
  (unit, Client.error) result

val patch :
  ?cancel:Cancel.t ->
  ?options:Client.write_options ->
  Client.t ->
  api:Core.api ->
  ?namespace:string ->
  string ->
  Client.patch ->
  (t, Client.error) result

val patch_status :
  ?cancel:Cancel.t ->
  ?options:Client.write_options ->
  Client.t ->
  api:Core.api ->
  ?namespace:string ->
  string ->
  Client.patch ->
  (t, Client.error) result

val replace_status :
  ?cancel:Cancel.t ->
  ?options:Client.write_options ->
  Client.t ->
  api:Core.api ->
  ?namespace:string ->
  string ->
  t ->
  (t, Client.error) result

val get_subresource :
  ?cancel:Cancel.t ->
  Client.t ->
  api:Core.api ->
  ?namespace:string ->
  string ->
  string ->
  (Yojson.Safe.t, Client.error) result

val create_subresource :
  ?cancel:Cancel.t ->
  ?options:Client.write_options ->
  Client.t ->
  api:Core.api ->
  ?namespace:string ->
  name:string ->
  subresource:string ->
  Yojson.Safe.t ->
  (Yojson.Safe.t, Client.error) result

val replace_subresource :
  ?cancel:Cancel.t ->
  ?options:Client.write_options ->
  Client.t ->
  api:Core.api ->
  ?namespace:string ->
  name:string ->
  subresource:string ->
  Yojson.Safe.t ->
  (Yojson.Safe.t, Client.error) result

val patch_subresource :
  ?cancel:Cancel.t ->
  ?options:Client.write_options ->
  Client.t ->
  api:Core.api ->
  ?namespace:string ->
  name:string ->
  subresource:string ->
  Client.patch ->
  (Yojson.Safe.t, Client.error) result

val delete_subresource :
  ?cancel:Cancel.t ->
  ?options:Client.delete_options ->
  Client.t ->
  api:Core.api ->
  ?namespace:string ->
  string ->
  string ->
  (unit, Client.error) result

val stream_subresource :
  ?cancel:Cancel.t ->
  ?query:(string * string) list ->
  ?max_error_body_bytes:int ->
  Client.t ->
  api:Core.api ->
  ?namespace:string ->
  string ->
  string ->
  on_chunk:(string -> unit) ->
  (unit, Client.error) result

val get_scale :
  ?cancel:Cancel.t ->
  Client.t ->
  api:Core.api ->
  ?namespace:string ->
  string ->
  (Client.scale, Client.error) result

val replace_scale :
  ?cancel:Cancel.t ->
  ?options:Client.write_options ->
  Client.t ->
  api:Core.api ->
  ?namespace:string ->
  string ->
  Client.scale ->
  (Client.scale, Client.error) result

val patch_scale :
  ?cancel:Cancel.t ->
  ?options:Client.write_options ->
  Client.t ->
  api:Core.api ->
  ?namespace:string ->
  string ->
  Client.patch ->
  (Client.scale, Client.error) result

val logs :
  ?cancel:Cancel.t ->
  ?options:Client.log_options ->
  ?max_body_bytes:int ->
  Client.t ->
  api:Core.api ->
  ?namespace:string ->
  string ->
  (string, Client.error) result

val stream_logs :
  ?cancel:Cancel.t ->
  ?options:Client.log_options ->
  ?max_error_body_bytes:int ->
  Client.t ->
  api:Core.api ->
  ?namespace:string ->
  string ->
  on_chunk:(string -> unit) ->
  (unit, Client.error) result

val list :
  ?cancel:Cancel.t ->
  ?namespace:string ->
  ?label_selector:string ->
  ?field_selector:string ->
  ?resource_version:Core.Resource_version.t ->
  ?resource_version_match:[ `Exact | `Not_older_than ] ->
  ?limit:int ->
  ?continue:string ->
  Client.t ->
  api:Core.api ->
  (t Client.list_result, Client.error) result

val list_all :
  ?cancel:Cancel.t ->
  ?namespace:string ->
  ?label_selector:string ->
  ?field_selector:string ->
  ?resource_version:Core.Resource_version.t ->
  ?resource_version_match:[ `Exact | `Not_older_than ] ->
  ?page_size:int ->
  Client.t ->
  api:Core.api ->
  (t Client.list_result, Client.error) result

val watch :
  ?cancel:Cancel.t ->
  ?namespace:string ->
  ?label_selector:string ->
  ?field_selector:string ->
  ?timeout_seconds:int ->
  ?allow_bookmarks:bool ->
  ?resource_version_match:[ `Exact | `Not_older_than ] ->
  ?send_initial_events:bool ->
  ?max_event_bytes:int ->
  Client.t ->
  api:Core.api ->
  resource_version:Core.Resource_version.t ->
  on_event:(t Client.watch_event -> unit) ->
  (Client.watch_outcome, Client.error) result
