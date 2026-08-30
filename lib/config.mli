(** Kubernetes connection and authentication configuration. *)

type exec

type credential =
  | Anonymous
  | Static_token of string
  | Token_file of string
  | Basic of { username : string; password : string }
  | Exec of exec

type tls = {
  ca_pem : string option;
  client_certificate_pem : string option;
  client_key_pem : string option;
  insecure_skip_verify : bool;
  server_name : string option;
}

type t = {
  server : Uri.t;
  namespace : string option;
  credential : credential;
  tls : tls;
}

val load_kubeconfig : ?context:string -> string -> (t, string) result
(** Load one kubeconfig and select its current or explicitly named context. *)

val load_kubeconfigs : ?context:string -> string list -> (t, string) result
(** Merge kubeconfigs using first-file-wins name resolution. Relative
    certificate, key, and token paths are resolved against the file that defines
    their entry. *)

val in_cluster : unit -> (t, string) result

val load_default : ?context:string -> unit -> (t, string) result
(** Prefer in-cluster configuration when service environment variables exist;
    otherwise load every path in [KUBECONFIG], or the standard user path. *)

val bearer_token : t -> (string option, string) result
(** Resolve only bearer-token credentials. Prefer [authorization_header] for
    general request code. *)

val authorization_header : t -> (string option, string) result
(** Resolve the current Authorization value, refreshing token files and expiring
    exec-plugin credentials when needed. *)

val invalidate_credential : t -> bool
(** Clear a refreshable credential after an authentication failure. Returns
    [true] when a refresh is possible. *)
