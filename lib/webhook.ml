type status = Idle | Starting | Listening of int | Failed of string | Stopped

type handler =
  cancel:Cancel.t -> Yojson.Safe.t -> (Yojson.Safe.t, string) result

type route_metrics = {
  requests : Metrics.Counter.t;
  failures : Metrics.Counter.t;
  duration : Metrics.Histogram.t;
}

type route = { handler : handler; metrics : route_metrics option }

type t = {
  address : string;
  requested_port : int;
  max_connections : int;
  max_header_bytes : int;
  max_body_bytes : int;
  request_timeout : float;
  tls_config : Tls.Config.server;
  metrics : Metrics.t option;
  routes : (string, route) Hashtbl.t;
  lock : Mutex.t;
  changed : Condition.t;
  mutable status : status;
  mutable manager_component : Manager.component option;
}

type connection = { thread : Thread.t; finished : bool Atomic.t }

let ( let* ) result fn =
  match result with
  | Ok value -> fn value
  | Error _ as error -> error

let decode_certificates label pem =
  match X509.Certificate.decode_pem_multiple pem with
  | Ok [] -> Error (label ^ " contains no certificates")
  | Ok certificates -> Ok certificates
  | Error (`Msg message) -> Error (label ^ ": " ^ message)

let tls_config ~certificate_pem ~private_key_pem client_ca_pem =
  Crypto_runtime.ensure_rng ();
  let* certificates = decode_certificates "certificate PEM" certificate_pem in
  let* private_key =
    match X509.Private_key.decode_pem private_key_pem with
    | Ok key -> Ok key
    | Error (`Msg message) -> Error ("private key PEM: " ^ message)
  in
  let* authenticator, acceptable_cas =
    match client_ca_pem with
    | None -> Ok (None, [])
    | Some pem ->
        let* anchors = decode_certificates "client CA PEM" pem in
        let authenticator =
          X509.Authenticator.chain_of_trust
            ~time:(fun () -> Some (Ptime_clock.now ()))
            anchors
        in
        Ok (Some authenticator, List.map X509.Certificate.subject anchors)
  in
  match
    Tls.Config.server ~version:(`TLS_1_2, `TLS_1_3)
      ~certificates:(`Single (certificates, private_key))
      ?authenticator ~acceptable_cas ~alpn_protocols:[ "http/1.1" ] ()
  with
  | Ok config -> Ok config
  | Error (`Msg message) -> Error ("TLS server configuration: " ^ message)

let positive name value =
  if value <= 0 then
    invalid_arg ("Webhook.create: " ^ name ^ " must be positive")

let create ?(address = "127.0.0.1") ?(port = 9443) ?(max_connections = 128)
    ?(max_header_bytes = 16 * 1024) ?(max_body_bytes = 2 * 1024 * 1024)
    ?(request_timeout = 10.) ?client_ca_pem ?metrics ~certificate_pem
    ~private_key_pem () =
  if port < 0 || port > 65535 then invalid_arg "Webhook.create: invalid port";
  positive "max_connections" max_connections;
  positive "max_header_bytes" max_header_bytes;
  positive "max_body_bytes" max_body_bytes;
  if (not (Float.is_finite request_timeout)) || request_timeout <= 0. then
    invalid_arg "Webhook.create: request_timeout must be finite and positive";
  let* tls_config =
    tls_config ~certificate_pem ~private_key_pem client_ca_pem
  in
  Ok
    {
      address;
      requested_port = port;
      max_connections;
      max_header_bytes;
      max_body_bytes;
      request_timeout;
      tls_config;
      metrics;
      routes = Hashtbl.create 7;
      lock = Mutex.create ();
      changed = Condition.create ();
      status = Idle;
      manager_component = None;
    }

let protect server fn =
  Mutex.lock server.lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock server.lock) fn

let set_status server status =
  protect server (fun () ->
      server.status <- status;
      Condition.broadcast server.changed)

let valid_path path =
  path <> ""
  && path.[0] = '/'
  && (not (String.contains path '?'))
  && not (String.contains path '#')

let route_metrics registry path =
  let labels = [ ("webhook", path) ] in
  {
    requests =
      Metrics.Counter.create ~registry ~name:"ocaml_kube_webhook_requests_total"
        ~help:"JSON review requests received by a configured webhook." ~labels
        ();
    failures =
      Metrics.Counter.create ~registry ~name:"ocaml_kube_webhook_failures_total"
        ~help:"JSON review requests rejected before a response was produced."
        ~labels ();
    duration =
      Metrics.Histogram.create ~registry
        ~name:"ocaml_kube_webhook_duration_seconds"
        ~help:"JSON review handling latency in seconds."
        ~buckets:[ 0.001; 0.005; 0.01; 0.025; 0.05; 0.1; 0.25; 0.5; 1.; 2.5 ]
        ~labels ();
  }

let add server ~path handler =
  if not (valid_path path) then invalid_arg "Webhook.add: invalid path";
  protect server (fun () ->
      if server.status <> Idle then
        invalid_arg "Webhook.add: server already started";
      if Hashtbl.mem server.routes path then
        invalid_arg ("Webhook.add: duplicate path " ^ path);
      let metrics =
        Option.map (fun registry -> route_metrics registry path) server.metrics
      in
      Hashtbl.add server.routes path { handler; metrics })

let add_admission server ~path handler =
  add server ~path (fun ~cancel review ->
      Admission.respond ~cancel handler review)

let add_conversion server ~path handler =
  add server ~path (fun ~cancel review ->
      Conversion.respond ~cancel handler review)

let find_boundary value =
  let rec loop index =
    if index + 4 > String.length value then None
    else if String.sub value index 4 = "\r\n\r\n" then Some index
    else loop (index + 1)
  in
  loop 0

let read_head server cancel flow =
  let output = Buffer.create 2048 in
  let buffer = Bytes.create 2048 in
  let rec loop () =
    if Cancel.is_cancelled cancel then Error "request cancelled"
    else
      let contents = Buffer.contents output in
      match find_boundary contents with
      | Some boundary when boundary > server.max_header_bytes ->
          Error "request headers too large"
      | Some boundary ->
          let body_offset = boundary + 4 in
          Ok
            ( String.sub contents 0 boundary,
              String.sub contents body_offset
                (String.length contents - body_offset) )
      | None when Buffer.length output > server.max_header_bytes ->
          Error "request headers too large"
      | None -> (
          try
            let count = Tls_unix.read flow buffer in
            if count = 0 then Error "connection closed before request headers"
            else (
              Buffer.add_subbytes output buffer 0 count;
              loop ())
          with Tls_unix.Closed_by_peer | End_of_file ->
            Error "connection closed before request headers")
  in
  loop ()

let trim_cr line =
  let length = String.length line in
  if length > 0 && line.[length - 1] = '\r' then String.sub line 0 (length - 1)
  else line

let token_char = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '!'
  | '#'
  | '$'
  | '%'
  | '&'
  | '\''
  | '*'
  | '+'
  | '-'
  | '.'
  | '^'
  | '_'
  | '`'
  | '|'
  | '~' -> true
  | _ -> false

let parse_headers head =
  match String.split_on_char '\n' head |> List.map trim_cr with
  | [] -> Error "empty request"
  | request_line :: header_lines -> (
      let request =
        String.split_on_char ' ' request_line
        |> List.filter (fun value -> value <> "")
      in
      match request with
      | [ meth; target; ("HTTP/1.1" | "HTTP/1.0") ] ->
          if target = "" || target.[0] <> '/' then
            Error "invalid request target"
          else
            let rec parse accumulator = function
              | [] -> Ok (meth, target, List.rev accumulator)
              | line :: rest -> (
                  if line = "" then parse accumulator rest
                  else if line.[0] = ' ' || line.[0] = '\t' then
                    Error "folded request headers are not supported"
                  else
                    match String.index_opt line ':' with
                    | None -> Error "malformed request header"
                    | Some index ->
                        let name = String.sub line 0 index in
                        if name = "" || not (String.for_all token_char name)
                        then Error "invalid request header name"
                        else
                          let value =
                            String.sub line (index + 1)
                              (String.length line - index - 1)
                            |> String.trim
                          in
                          parse
                            ((String.lowercase_ascii name, value) :: accumulator)
                            rest)
            in
            parse [] header_lines
      | _ -> Error "invalid HTTP request line")

let header_values name headers =
  List.filter_map
    (fun (candidate, value) -> if candidate = name then Some value else None)
    headers

let one_header name headers =
  match header_values name headers with
  | [] -> Ok None
  | [ value ] -> Ok (Some value)
  | _ -> Error ("duplicate " ^ name ^ " header")

let parse_content_length server headers =
  match one_header "content-length" headers with
  | Error message -> Error (400, "Bad Request", message)
  | Ok None -> Error (411, "Length Required", "Content-Length is required")
  | Ok (Some value) -> (
      match int_of_string_opt value with
      | Some length when length >= 0 && length <= server.max_body_bytes ->
          Ok length
      | Some length when length > server.max_body_bytes ->
          Error (413, "Payload Too Large", "request body too large")
      | Some _ | None -> Error (400, "Bad Request", "invalid Content-Length"))

let media_type value =
  match String.split_on_char ';' value with
  | first :: _ -> String.lowercase_ascii (String.trim first)
  | [] -> ""

let validate_entity_headers server headers =
  match
    ( one_header "transfer-encoding" headers,
      one_header "content-encoding" headers,
      one_header "content-type" headers )
  with
  | Error message, _, _ | _, Error message, _ | _, _, Error message ->
      Error (400, "Bad Request", message)
  | Ok (Some _), _, _ ->
      Error (415, "Unsupported Media Type", "Transfer-Encoding is not supported")
  | _, Ok (Some _), _ ->
      Error (415, "Unsupported Media Type", "Content-Encoding is not supported")
  | _, _, Ok (Some value) when media_type value = "application/json" ->
      parse_content_length server headers
  | _ ->
      Error
        (415, "Unsupported Media Type", "Content-Type must be application/json")

let write flow value = Tls_unix.write flow value

let response ?(content_type = "text/plain; charset=utf-8") status reason body =
  Printf.sprintf
    "HTTP/1.1 %d %s\r\n\
     Content-Type: %s\r\n\
     Content-Length: %d\r\n\
     Cache-Control: no-store\r\n\
     X-Content-Type-Options: nosniff\r\n\
     Connection: close\r\n\
     \r\n\
     %s"
    status reason content_type (String.length body) body

let read_body cancel flow initial length =
  if String.length initial >= length then Ok (String.sub initial 0 length)
  else
    let output = Bytes.create length in
    Bytes.blit_string initial 0 output 0 (String.length initial);
    let rec loop offset =
      if offset = length then Ok (Bytes.unsafe_to_string output)
      else if Cancel.is_cancelled cancel then Error "request cancelled"
      else
        try
          let count =
            Tls_unix.read flow ~off:offset ~len:(length - offset) output
          in
          if count = 0 then Error "truncated request body"
          else loop (offset + count)
        with Tls_unix.Closed_by_peer | End_of_file ->
          Error "truncated request body"
    in
    loop (String.length initial)

let request_path target =
  match String.index_opt target '?' with
  | None -> target
  | Some index -> String.sub target 0 index

let json_of_string value =
  try Ok (Yojson.Safe.from_string value)
  with Yojson.Json_error message -> Error ("invalid JSON: " ^ message)

let record_failure (route : route) =
  Option.iter
    (fun metrics -> Metrics.Counter.inc metrics.failures)
    route.metrics

let route_request server cancel flow (route : route) headers initial =
  let started = Clock.now () in
  Option.iter
    (fun metrics -> Metrics.Counter.inc metrics.requests)
    route.metrics;
  let finish () =
    Option.iter
      (fun metrics ->
        Metrics.Histogram.observe metrics.duration (Clock.now () -. started))
      route.metrics
  in
  Fun.protect ~finally:finish (fun () ->
      match validate_entity_headers server headers with
      | Error (status, reason, message) ->
          record_failure route;
          response status reason (message ^ "\n")
      | Ok length ->
          let expect = header_values "expect" headers in
          if expect <> [] && expect <> [ "100-continue" ] then (
            record_failure route;
            response 417 "Expectation Failed" "unsupported expectation\n")
          else (
            if expect <> [] then write flow "HTTP/1.1 100 Continue\r\n\r\n";
            match read_body cancel flow initial length with
            | Error message ->
                record_failure route;
                response 400 "Bad Request" (message ^ "\n")
            | Ok body -> (
                match json_of_string body with
                | Error message ->
                    record_failure route;
                    response 400 "Bad Request" (message ^ "\n")
                | Ok review -> (
                    match
                      try `Result (route.handler ~cancel review)
                      with exn -> `Exception (Printexc.to_string exn)
                    with
                    | `Exception message ->
                        record_failure route;
                        response 500 "Internal Server Error"
                          ("uncaught webhook handler exception: " ^ message
                         ^ "\n")
                    | `Result (Error message) ->
                        record_failure route;
                        response 400 "Bad Request" (message ^ "\n")
                    | `Result (Ok review) ->
                        let body = Yojson.Safe.to_string review in
                        response ~content_type:"application/json" 200 "OK" body)
                )))

let route server cancel flow meth target headers initial =
  if meth <> "POST" then
    response 405 "Method Not Allowed" "method not allowed\n"
  else
    let path = request_path target in
    match Hashtbl.find_opt server.routes path with
    | None -> response 404 "Not Found" "not found\n"
    | Some route -> route_request server cancel flow route headers initial

let handle_http server cancel flow =
  match read_head server cancel flow with
  | Error message -> write flow (response 400 "Bad Request" (message ^ "\n"))
  | Ok (head, initial) -> (
      match parse_headers head with
      | Error message ->
          write flow (response 400 "Bad Request" (message ^ "\n"))
      | Ok (meth, target, headers) ->
          write flow (route server cancel flow meth target headers initial))

let close_fd fd = try Unix.close fd with Unix.Unix_error _ -> ()

let handle_connection server parent fd =
  ignore
    (Cancel.with_timeout ~parent server.request_timeout (fun cancel ->
         let interrupt_lock = Mutex.create () in
         let interruptible = ref true in
         let interrupt () =
           Mutex.lock interrupt_lock;
           (if !interruptible then
              try Unix.shutdown fd Unix.SHUTDOWN_ALL
              with Unix.Unix_error _ -> ());
           Mutex.unlock interrupt_lock
         in
         let unregister = Cancel.on_cancel cancel interrupt in
         let flow = ref None in
         Fun.protect
           ~finally:(fun () ->
             unregister ();
             Mutex.lock interrupt_lock;
             interruptible := false;
             Mutex.unlock interrupt_lock;
             match !flow with
             | Some flow -> ( try Tls_unix.close flow with _ -> ())
             | None -> close_fd fd)
           (fun () ->
             try
               let established = Tls_unix.server_of_fd server.tls_config fd in
               flow := Some established;
               handle_http server cancel established
             with
             | Tls_unix.Tls_alert _
             | Tls_unix.Tls_failure _
             | Tls_unix.Closed_by_peer
             | End_of_file
             | Unix.Unix_error _
             ->
               ())))

let reap connections =
  let finished, running =
    List.partition
      (fun connection -> Atomic.get connection.finished)
      connections
  in
  List.iter (fun connection -> Thread.join connection.thread) finished;
  running

let start_connection server cancel fd =
  let finished = Atomic.make false in
  let thread =
    Thread.create
      (fun () ->
        Fun.protect
          ~finally:(fun () -> Atomic.set finished true)
          (fun () -> handle_connection server cancel fd))
      ()
  in
  { thread; finished }

let begin_start server =
  protect server (fun () ->
      match server.status with
      | Idle when Hashtbl.length server.routes = 0 ->
          Error "webhook server has no registered paths"
      | Idle ->
          server.status <- Starting;
          Condition.broadcast server.changed;
          Ok ()
      | Starting | Listening _ | Failed _ | Stopped ->
          Error "webhook server can only be started once")

let run server ~client ~cancel =
  let logger =
    Log.with_name (Client.logger client) "webhook" |> fun logger ->
    Log.with_fields logger
      [
        ("address", Log.String server.address);
        ("requested_port", Log.Int server.requested_port);
      ]
  in
  match begin_start server with
  | Error message ->
      set_status server (Failed message);
      Log.error logger
        ~fields:[ ("error", Log.String message) ]
        "Webhook server failed to start";
      Error (Client.Invalid_request message)
  | Ok () -> (
      try
        let address = Unix.inet_addr_of_string server.address in
        let domain =
          if String.contains server.address ':' then Unix.PF_INET6
          else Unix.PF_INET
        in
        let listener = Unix.socket ~cloexec:true domain Unix.SOCK_STREAM 0 in
        Fun.protect
          ~finally:(fun () -> close_fd listener)
          (fun () ->
            Unix.setsockopt listener Unix.SO_REUSEADDR true;
            Unix.bind listener (Unix.ADDR_INET (address, server.requested_port));
            Unix.listen listener server.max_connections;
            let port =
              match Unix.getsockname listener with
              | Unix.ADDR_INET (_, port) -> port
              | Unix.ADDR_UNIX _ -> assert false
            in
            set_status server (Listening port);
            Log.info logger
              ~fields:
                [
                  ("port", Log.Int port);
                  ("routes", Log.Int (Hashtbl.length server.routes));
                ]
              "Webhook server listening";
            let connections = ref [] in
            while not (Cancel.is_cancelled cancel) do
              connections := reap !connections;
              let readable, _, _ = Unix.select [ listener ] [] [] 0.2 in
              if readable <> [] then
                let fd, _ = Unix.accept ~cloexec:true listener in
                if List.length !connections >= server.max_connections then
                  close_fd fd
                else
                  connections :=
                    start_connection server cancel fd :: !connections
            done;
            List.iter
              (fun connection -> Thread.join connection.thread)
              !connections;
            set_status server Stopped;
            Log.info logger "Webhook server stopped";
            Ok ())
      with
      | Unix.Unix_error (code, fn, argument) ->
          let message =
            Printf.sprintf "%s(%s): %s" fn argument (Unix.error_message code)
          in
          set_status server (Failed message);
          Log.error logger
            ~fields:[ ("error", Log.String message) ]
            "Webhook server failed";
          Error (Client.Transport message)
      | exn ->
          let message = Printexc.to_string exn in
          set_status server (Failed message);
          Log.error logger
            ~fields:[ ("error", Log.String message) ]
            "Webhook server failed";
          Error (Client.Transport message))

let component server =
  protect server (fun () ->
      match server.manager_component with
      | Some component -> component
      | None ->
          let component =
            Manager.component
              ~name:
                (Printf.sprintf "webhook/%s:%d" server.address
                   server.requested_port) (fun ~client ~cancel ->
                run server ~client ~cancel)
          in
          server.manager_component <- Some component;
          component)

let bound_port server =
  protect server (fun () ->
      match server.status with
      | Listening port -> Some port
      | _ -> None)

let await_listening ~cancel server =
  let unregister =
    Cancel.on_cancel cancel (fun () ->
        protect server (fun () -> Condition.broadcast server.changed))
  in
  let result =
    protect server (fun () ->
        while
          (server.status = Idle || server.status = Starting)
          && not (Cancel.is_cancelled cancel)
        do
          Condition.wait server.changed server.lock
        done;
        match server.status with
        | Listening port -> Ok port
        | Failed message -> Error message
        | Idle | Starting | Stopped ->
            Error "webhook server stopped before listening")
  in
  unregister ();
  result

let readiness_check server () =
  protect server (fun () ->
      match server.status with
      | Listening _ -> Ok ()
      | Idle -> Error "webhook server has not started"
      | Starting -> Error "webhook server is starting"
      | Failed message -> Error message
      | Stopped -> Error "webhook server has stopped")
