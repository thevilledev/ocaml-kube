(** Kubernetes API group and resource discovery. *)

type resource = {
  name : string;
  singular_name : string;
  namespaced : bool;
  kind : string;
  verbs : string list;
  short_names : string list;
  categories : string list;
}

type resource_list = { group_version : string; resources : resource list }
type group_version = { group_version : string; version : string }

type group = {
  name : string;
  versions : group_version list;
  preferred_version : group_version option;
}

val server_version :
  ?cancel:Cancel.t -> Client.t -> (Yojson.Safe.t, Client.error) result

val core_versions :
  ?cancel:Cancel.t -> Client.t -> (string list, Client.error) result

val groups : ?cancel:Cancel.t -> Client.t -> (group list, Client.error) result

val resources :
  ?cancel:Cancel.t ->
  Client.t ->
  group:string ->
  version:string ->
  (resource_list, Client.error) result
