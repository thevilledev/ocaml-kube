(** Deterministic loopback HTTP server for exercising the real Kubernetes client
    transport under scripted response and framing failures. *)

type t
type response

val fixed :
  ?status:string -> ?headers:(string * string) list -> string -> response
(** A complete Content-Length-framed response. *)

val chunked :
  ?status:string ->
  ?headers:(string * string) list ->
  ?trailers:(string * string) list ->
  ?terminate:bool ->
  string list ->
  response
(** A chunked response whose list elements are distinct HTTP chunks. Setting
    [terminate] to [false] closes the connection before the terminal zero-size
    chunk. *)

val raw : string list -> response
(** Raw response fragments, for malformed or deliberately truncated framing. *)

val delayed : float -> response -> response
(** Delay response transmission after accepting and capturing the request. This
    is intended for deterministic in-flight concurrency tests. *)

val with_server : response list -> (t -> 'a) -> 'a
(** Run a callback against one fresh connection per scripted response. The
    callback must consume every response. Server exceptions and unconsumed
    scripts fail the test instead of leaving a background thread behind. *)

val config : t -> Kube.Config.t

val requests : t -> string list
(** Complete captured requests in arrival order. *)
