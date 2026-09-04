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

type impersonation

val default_tls : tls

val make_impersonation :
  ?uid:string ->
  ?groups:string list ->
  ?extra:(string * string list) list ->
  user:string ->
  unit ->
  (impersonation, string) result
(** Validate Kubernetes user impersonation state. Extra keys must be lowercase;
    their header suffixes are percent-escaped when requests are prepared. *)

type t = {
  server : Uri.t;
  namespace : string option;
  credential : credential;
  tls : tls;
  proxy_url : Uri.t option;
  impersonation : impersonation option;
}

val make :
  ?namespace:string ->
  ?credential:credential ->
  ?tls:tls ->
  ?proxy_url:Uri.t ->
  ?impersonation:impersonation ->
  Uri.t ->
  t
(** Construct a validated client configuration. Explicit proxy URLs may use the
    [http], [https], or [socks5] kubeconfig schemes; transport support is
    checked when a connection is opened. *)

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

type credential_origin = [ `Protected | `Heap ]
(** Whether the credential bytes originated outside the OCaml heap. Inline,
    basic, and exec-plugin credentials are necessarily [`Heap]; token files are
    read directly into protected memory and are [`Protected]. *)

val with_authorization_secret :
  ?hardened:bool ->
  t ->
  (origin:credential_origin -> Secret.t option -> 'a) ->
  ('a, string) result
(** Resolve the current Authorization value into a scoped [Secret.t]. The value
    is destroyed when the callback returns or raises. Token files are read with
    [Secret_unix] without first creating an OCaml string. Other credential kinds
    remain available for compatibility but are marked as heap-originating so
    security-sensitive callers can reject them. *)

val impersonation_headers : t -> (string * string) list
(** Return the validated Kubernetes impersonation headers for this config. *)

val invalidate_credential : t -> bool
(** Clear a refreshable credential after an authentication failure. Returns
    [true] when a refresh is possible. *)
