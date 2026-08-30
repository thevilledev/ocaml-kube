(** Blocking, cancellation-aware HTTP/1.1 transport for the Kubernetes API.
    Successful streaming responses invoke [on_chunk] without retaining the body.
    Non-streaming and error responses are bounded by [max_body_bytes]. The
    transport owns Host, Content-Length, Transfer-Encoding, and Connection. *)

type meth = [ `GET | `POST | `PUT | `PATCH | `DELETE ]

type response = {
  status : int;
  reason : string;
  headers : (string * string) list;
  body : string;
}

val request :
  ?cancel:Cancel.t ->
  ?headers:(string * string) list ->
  ?body:string ->
  ?on_chunk:(string -> unit) ->
  ?max_body_bytes:int ->
  Config.t ->
  meth ->
  string ->
  (response, string) result
