module Transport = struct
  type request = {
    meth : Kube.Http.meth;
    target : string;
    headers : (string * string) list;
    body : string option;
    streaming : bool;
    max_body_bytes : int option;
  }

  type reply = {
    status : int;
    reason : string;
    headers : (string * string) list;
    chunks : string list;
    wait_for_cancel : bool;
  }

  type outcome = Reply of reply | Failure of string

  let default_reason = function
    | 200 -> "OK"
    | 201 -> "Created"
    | 202 -> "Accepted"
    | 204 -> "No Content"
    | 400 -> "Bad Request"
    | 401 -> "Unauthorized"
    | 403 -> "Forbidden"
    | 404 -> "Not Found"
    | 409 -> "Conflict"
    | 410 -> "Gone"
    | 422 -> "Unprocessable Entity"
    | 429 -> "Too Many Requests"
    | 500 -> "Internal Server Error"
    | 503 -> "Service Unavailable"
    | _ -> ""

  let validate_status status =
    if status < 100 || status > 599 then
      invalid_arg "Kube_test.Transport: status must be between 100 and 599"

  let make_reply ?(status = 200) ?reason ?(headers = [])
      ?(wait_for_cancel = false) chunks =
    validate_status status;
    Reply
      {
        status;
        reason = Option.value ~default:(default_reason status) reason;
        headers;
        chunks;
        wait_for_cancel;
      }

  let respond ?status ?reason ?headers body =
    make_reply ?status ?reason ?headers [ body ]

  let respond_json ?status ?reason ?(headers = []) json =
    respond ?status ?reason
      ~headers:
        (if
           List.exists
             (fun (name, _) -> String.lowercase_ascii name = "content-type")
             headers
         then headers
         else ("Content-Type", "application/json") :: headers)
      (Yojson.Safe.to_string json)

  let stream ?status ?reason ?headers ?wait_for_cancel chunks =
    make_reply ?status ?reason ?headers ?wait_for_cancel chunks

  let fail message = Failure message

  type source = Handler of (request -> outcome) | Scripted of outcome Queue.t

  type state = {
    lock : Mutex.t;
    source : source;
    mutable requests_rev : request list;
    mutable unexpected_rev : string list;
    closed : bool Atomic.t;
  }

  type t = { state : state; transport : Kube.Client.Transport.t }

  let capture (request : Kube.Client.Transport.request) =
    {
      meth = request.meth;
      target = request.target;
      headers = request.headers;
      body = request.body;
      streaming = Option.is_some request.on_chunk;
      max_body_bytes = request.max_body_bytes;
    }

  let cancelled (request : Kube.Client.Transport.request) =
    Option.fold ~none:false ~some:Kube.Cancel.is_cancelled request.cancel

  let body_limit (request : Kube.Client.Transport.request) =
    Option.value ~default:(32 * 1024 * 1024) request.max_body_bytes

  let deliver request = function
    | Failure message -> Error message
    | Reply reply -> (
        let limit = body_limit request in
        if limit < 0 then Error "max_body_bytes must not be negative"
        else if cancelled request then Error "request cancelled"
        else
          let successful = reply.status >= 200 && reply.status < 300 in
          let stream_success = successful && Option.is_some request.on_chunk in
          let no_body =
            (reply.status >= 100 && reply.status < 200)
            || reply.status = 204 || reply.status = 304
          in
          let chunks = if no_body then [] else reply.chunks in
          let body = Buffer.create 256 in
          let buffered = ref 0 in
          let rec consume = function
            | [] -> Ok ()
            | chunk :: rest -> (
                if cancelled request then Error "request cancelled"
                else if not stream_success then (
                  buffered := !buffered + String.length chunk;
                  if !buffered > limit then
                    Error (Printf.sprintf "HTTP body exceeds %d bytes" limit)
                  else (
                    Buffer.add_string body chunk;
                    consume rest))
                else
                  match request.on_chunk with
                  | None -> assert false
                  | Some callback -> (
                      try
                        callback chunk;
                        consume rest
                      with exn ->
                        Error
                          ("stream callback raised: " ^ Printexc.to_string exn))
                )
          in
          match consume chunks with
          | Error _ as error -> error
          | Ok () ->
              if reply.wait_for_cancel then
                match request.cancel with
                | None ->
                    Error
                      "wait_for_cancel response requires a request \
                       cancellation token"
                | Some cancel ->
                    if Kube.Cancel.sleep cancel 86_400. then
                      Error
                        "wait_for_cancel response reached its safety timeout"
                    else Error "request cancelled"
              else
                Ok
                  {
                    Kube.Http.status = reply.status;
                    reason = reply.reason;
                    headers = reply.headers;
                    body = Buffer.contents body;
                  })

  let source_outcome state request =
    Mutex.lock state.lock;
    let captured = capture request in
    state.requests_rev <- captured :: state.requests_rev;
    let outcome =
      match state.source with
      | Handler handler -> `Handler handler
      | Scripted queue ->
          if Queue.is_empty queue then (
            let message =
              Printf.sprintf "unexpected request %s"
                (match captured.meth with
                | `GET -> "GET " ^ captured.target
                | `POST -> "POST " ^ captured.target
                | `PUT -> "PUT " ^ captured.target
                | `PATCH -> "PATCH " ^ captured.target
                | `DELETE -> "DELETE " ^ captured.target)
            in
            state.unexpected_rev <- message :: state.unexpected_rev;
            `Outcome (Failure message))
          else `Outcome (Queue.pop queue)
    in
    Mutex.unlock state.lock;
    match outcome with
    | `Outcome outcome -> outcome
    | `Handler handler -> handler captured

  let make source =
    let state =
      {
        lock = Mutex.create ();
        source;
        requests_rev = [];
        unexpected_rev = [];
        closed = Atomic.make false;
      }
    in
    let transport =
      Kube.Client.Transport.make
        ~close:(fun () -> Atomic.set state.closed true)
        (fun request -> source_outcome state request |> deliver request)
    in
    { state; transport }

  let create handler = make (Handler handler)

  let scripted outcomes =
    let queue = Queue.create () in
    List.iter (fun outcome -> Queue.push outcome queue) outcomes;
    make (Scripted queue)

  let client_transport test = test.transport

  let protect state fn =
    Mutex.lock state.lock;
    Fun.protect ~finally:(fun () -> Mutex.unlock state.lock) fn

  let requests test =
    protect test.state (fun () -> List.rev test.state.requests_rev)

  let request_count test =
    protect test.state (fun () -> List.length test.state.requests_rev)

  let remaining test =
    protect test.state (fun () ->
        match test.state.source with
        | Handler _ -> None
        | Scripted queue -> Some (Queue.length queue))

  let is_closed test = Atomic.get test.state.closed

  let verify_complete test =
    protect test.state (fun () ->
        match (test.state.source, List.rev test.state.unexpected_rev) with
        | Handler _, _ -> Ok ()
        | Scripted _, message :: _ -> Error message
        | Scripted queue, [] when Queue.is_empty queue -> Ok ()
        | Scripted queue, [] ->
            Error
              (Printf.sprintf "%d scripted response(s) were not consumed"
                 (Queue.length queue)))

  let header (request : request) name =
    let name = String.lowercase_ascii name in
    List.find_map
      (fun (candidate, value) ->
        if String.lowercase_ascii candidate = name then Some value else None)
      request.headers
end

let config ?(server = Uri.of_string "https://kubernetes.test") ?namespace
    ?(credential = Kube.Config.Anonymous) () =
  Kube.Config.make ?namespace ~credential server

let client ?configuration ?rate_limiter ?logger transport =
  let configuration = Option.value ~default:(config ()) configuration in
  Kube.Client.create_with_transport ?rate_limiter ?logger
    ~transport:(Transport.client_transport transport)
    configuration
