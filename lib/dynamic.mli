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

val delete :
  ?cancel:Cancel.t ->
  ?options:Client.delete_options ->
  Client.t ->
  api:Core.api ->
  ?namespace:string ->
  string ->
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
