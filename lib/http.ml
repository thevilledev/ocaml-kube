type meth = [ `GET | `POST | `PUT | `PATCH | `DELETE ]

type response = {
  status : int;
  reason : string;
  headers : (string * string) list;
  body : string;
}

let () = Sys.set_signal Sys.sigpipe Sys.Signal_ignore

type flow = Plain of Unix.file_descr | Tls of Tls_unix.t

type t = {
  config : Config.t;
  max_idle_connections : int;
  connect_timeout : float;
  write_timeout : float;
  response_header_timeout : float;
  lock : Mutex.t;
  mutable idle : flow list;
  mutable closed : bool;
}

exception Body_too_large of int
exception Request_phase_timeout of string * float

let ( let* ) result fn =
  match result with
  | Ok value -> fn value
  | Error _ as error -> error

let method_string = function
  | `GET -> "GET"
  | `POST -> "POST"
  | `PUT -> "PUT"
  | `PATCH -> "PATCH"
  | `DELETE -> "DELETE"

let read flow buffer offset length =
  match flow with
  | Plain fd -> Unix.read fd buffer offset length
  | Tls tls -> Tls_unix.read tls ~off:offset ~len:length buffer

let rec write_plain fd value offset =
  if offset < String.length value then
    let written =
      Unix.write_substring fd value offset (String.length value - offset)
    in
    if written = 0 then raise End_of_file
    else write_plain fd value (offset + written)

let write flow value =
  match flow with
  | Plain fd -> write_plain fd value 0
  | Tls tls -> Tls_unix.write tls value

let read_flow = read
let write_flow = write

let close_flow = function
  | Plain fd -> ( try Unix.close fd with Unix.Unix_error _ -> ())
  | Tls tls -> ( try Tls_unix.close tls with _ -> ())

let flow_fd = function
  | Plain fd -> fd
  | Tls tls -> Tls_unix.file_descr tls

module Upgrade = struct
  type t = {
    flow : flow;
    pending : string;
    mutable pending_offset : int;
    read_lock : Mutex.t;
    write_lock : Mutex.t;
    closed : bool Atomic.t;
    mutable unregister : unit -> unit;
    cancel : Cancel.t option;
  }

  let terminate connection =
    if Atomic.compare_and_set connection.closed false true then (
      connection.unregister ();
      let fd = flow_fd connection.flow in
      (try Unix.shutdown fd Unix.SHUTDOWN_ALL with Unix.Unix_error _ -> ());
      close_flow connection.flow)

  let create ?cancel flow pending =
    let connection =
      {
        flow;
        pending;
        pending_offset = 0;
        read_lock = Mutex.create ();
        write_lock = Mutex.create ();
        closed = Atomic.make false;
        unregister = Fun.id;
        cancel;
      }
    in
    connection.unregister <-
      Option.fold ~none:Fun.id
        ~some:(fun token ->
          Cancel.on_cancel token (fun () -> terminate connection))
        cancel;
    connection

  let is_closed connection = Atomic.get connection.closed

  let cancelled connection =
    Option.fold ~none:false ~some:Cancel.is_cancelled connection.cancel

  let protect_io connection lock fn =
    if is_closed connection then Error "upgraded connection is closed"
    else (
      Mutex.lock lock;
      Fun.protect
        ~finally:(fun () -> Mutex.unlock lock)
        (fun () ->
          if is_closed connection then Error "upgraded connection is closed"
          else
            try Ok (fn ()) with
            | Unix.Unix_error (code, name, argument) ->
                if cancelled connection then
                  Error "upgraded connection cancelled"
                else
                  Error
                    (Printf.sprintf "%s(%s): %s" name argument
                       (Unix.error_message code))
            | Tls_unix.Tls_alert _ ->
                Error "TLS alert received on upgraded connection"
            | Tls_unix.Tls_failure failure ->
                Error
                  (Format.asprintf "TLS failure on upgraded connection: %a"
                     Tls.Engine.pp_failure failure)
            | Tls_unix.Closed_by_peer | End_of_file ->
                Error "upgraded connection closed by peer"
            | exn -> Error (Printexc.to_string exn)))

  let read connection buffer offset length =
    if offset < 0 || length < 0 || offset > Bytes.length buffer - length then
      invalid_arg "Http.Upgrade.read: invalid byte range";
    if length = 0 then Ok 0
    else
      protect_io connection connection.read_lock (fun () ->
          let available =
            String.length connection.pending - connection.pending_offset
          in
          if available > 0 then (
            let count = min available length in
            Bytes.blit_string connection.pending connection.pending_offset
              buffer offset count;
            connection.pending_offset <- connection.pending_offset + count;
            count)
          else read_flow connection.flow buffer offset length)

  let write connection value =
    protect_io connection connection.write_lock (fun () ->
        write_flow connection.flow value)

  let close = terminate
end

let valid_timeout value = Float.is_finite value && value > 0.

type 'a phase_outcome = Returned of 'a | Raised of exn

let with_phase_timeout ?cancel ~phase ~timeout flow fn =
  let parent = Option.value ~default:(Cancel.create ()) cancel in
  let outcome, timed_out =
    Cancel.with_timeout ~parent timeout (fun phase_cancel ->
        let fd = flow_fd flow in
        let unregister =
          Cancel.on_cancel phase_cancel (fun () ->
              try Unix.shutdown fd Unix.SHUTDOWN_ALL
              with Unix.Unix_error _ -> ())
        in
        Fun.protect ~finally:unregister (fun () ->
            try Returned (fn ()) with exn -> Raised exn))
  in
  if timed_out then raise (Request_phase_timeout (phase, timeout))
  else
    match outcome with
    | Returned value -> value
    | Raised exn -> raise exn

let create ?(max_idle_connections = 8) ?(connect_timeout = 10.)
    ?(write_timeout = 30.) ?(response_header_timeout = 30.) config =
  if max_idle_connections < 0 then
    invalid_arg "Http.create: max_idle_connections must not be negative";
  if not (valid_timeout connect_timeout) then
    invalid_arg "Http.create: connect_timeout must be finite and positive";
  if not (valid_timeout write_timeout) then
    invalid_arg "Http.create: write_timeout must be finite and positive";
  if not (valid_timeout response_header_timeout) then
    invalid_arg
      "Http.create: response_header_timeout must be finite and positive";
  {
    config;
    max_idle_connections;
    connect_timeout;
    write_timeout;
    response_header_timeout;
    lock = Mutex.create ();
    idle = [];
    closed = false;
  }

let close transport =
  Mutex.lock transport.lock;
  let idle = transport.idle in
  transport.idle <- [];
  transport.closed <- true;
  Mutex.unlock transport.lock;
  List.iter close_flow idle

let flow_is_idle flow =
  try
    let readable, _, _ = Unix.select [ flow_fd flow ] [] [] 0. in
    readable = []
  with Unix.Unix_error _ -> false

let unix_error code fn argument =
  Printf.sprintf "%s(%s): %s" fn argument (Unix.error_message code)

let resolve cancel host port =
  let wake_read, wake_write = Unix.pipe ~cloexec:true () in
  let lock = Mutex.create () in
  let result = ref None in
  let publish value =
    Mutex.lock lock;
    result := Some value;
    Mutex.unlock lock;
    try ignore (Unix.write_substring wake_write "x" 0 1)
    with Unix.Unix_error _ -> ()
  in
  let worker =
    try
      Thread.create
        (fun () ->
          Fun.protect
            ~finally:(fun () ->
              try Unix.close wake_write with Unix.Unix_error _ -> ())
            (fun () ->
              publish
                (try
                   Ok
                     (Unix.getaddrinfo host (string_of_int port)
                        [ Unix.AI_SOCKTYPE Unix.SOCK_STREAM ])
                 with Unix.Unix_error (code, fn, argument) ->
                   Error (unix_error code fn argument))))
        ()
    with exn ->
      (try Unix.close wake_read with Unix.Unix_error _ -> ());
      (try Unix.close wake_write with Unix.Unix_error _ -> ());
      raise exn
  in
  let rec wait () =
    if Cancel.is_cancelled cancel then
      Error "request cancelled during DNS lookup"
    else
      try
        let readable, _, _ = Unix.select [ wake_read ] [] [] 0.05 in
        if readable = [] then wait ()
        else (
          Thread.join worker;
          Mutex.lock lock;
          let value = !result in
          Mutex.unlock lock;
          match value with
          | Some value -> value
          | None -> Error "DNS lookup completed without a result")
      with Unix.Unix_error (Unix.EINTR, _, _) -> wait ()
  in
  Fun.protect
    ~finally:(fun () -> try Unix.close wake_read with Unix.Unix_error _ -> ())
    wait

let connect_address cancel address =
  let fd =
    Unix.socket address.Unix.ai_family address.ai_socktype address.ai_protocol
  in
  let fail message =
    (try Unix.close fd with Unix.Unix_error _ -> ());
    Error message
  in
  try
    Unix.set_close_on_exec fd;
    Unix.set_nonblock fd;
    let connected =
      try
        Unix.connect fd address.ai_addr;
        true
      with
      | Unix.Unix_error
          ((Unix.EINPROGRESS | Unix.EALREADY | Unix.EWOULDBLOCK), _, _) -> false
      | Unix.Unix_error (Unix.EISCONN, _, _) -> true
    in
    let rec await () =
      if Cancel.is_cancelled cancel then
        fail "request cancelled during TCP connect"
      else
        try
          let _, writable, exceptional = Unix.select [] [ fd ] [ fd ] 0.05 in
          if writable = [] && exceptional = [] then await ()
          else
            match Unix.getsockopt_error fd with
            | None ->
                Unix.clear_nonblock fd;
                (try Unix.setsockopt fd Unix.TCP_NODELAY true
                 with Unix.Unix_error _ -> ());
                Ok fd
            | Some code -> fail (unix_error code "connect" "")
        with Unix.Unix_error (Unix.EINTR, _, _) -> await ()
    in
    if connected then (
      Unix.clear_nonblock fd;
      (try Unix.setsockopt fd Unix.TCP_NODELAY true
       with Unix.Unix_error _ -> ());
      Ok fd)
    else await ()
  with Unix.Unix_error (code, fn, argument) ->
    fail (unix_error code fn argument)

let connect cancel host port =
  let* addresses = resolve cancel host port in
  let rec attempt errors = function
    | [] ->
        Error
          (match errors with
          | [] -> "no address found for " ^ host
          | message :: _ -> message)
    | _ when Cancel.is_cancelled cancel ->
        Error "request cancelled during connection establishment"
    | address :: rest -> (
        match connect_address cancel address with
        | Ok _ as connected -> connected
        | Error message -> attempt (message :: errors) rest)
  in
  attempt [] addresses

let endpoint_authority host port =
  let host = if String.contains host ':' then "[" ^ host ^ "]" else host in
  host ^ ":" ^ string_of_int port

let proxy_credentials proxy =
  match Uri.user proxy with
  | None -> None
  | Some user -> Some (user, Option.value ~default:"" (Uri.password proxy))

let proxy_authorization proxy =
  Option.map
    (fun (user, password) ->
      "Basic " ^ Base64.encode_string (user ^ ":" ^ password))
    (proxy_credentials proxy)

let read_exact_fd fd length =
  let value = Bytes.create length in
  let rec loop offset =
    if offset = length then Ok (Bytes.unsafe_to_string value)
    else
      let count = Unix.read fd value offset (length - offset) in
      if count = 0 then Error "proxy closed the connection during handshake"
      else loop (offset + count)
  in
  loop 0

let find_header_boundary value =
  let rec loop index =
    if index + 4 > String.length value then None
    else if String.sub value index 4 = "\r\n\r\n" then Some index
    else loop (index + 1)
  in
  loop 0

let read_proxy_response_head fd =
  let output = Buffer.create 512 in
  let chunk = Bytes.create 4096 in
  let rec loop () =
    if Buffer.length output > 64 * 1024 then
      Error "proxy response headers exceed 64 KiB"
    else
      let contents = Buffer.contents output in
      match find_header_boundary contents with
      | Some boundary ->
          Ok
            ( String.sub contents 0 boundary,
              String.sub contents (boundary + 4)
                (String.length contents - boundary - 4) )
      | None ->
          let count = Unix.read fd chunk 0 (Bytes.length chunk) in
          if count = 0 then Error "proxy closed before sending response headers"
          else (
            Buffer.add_subbytes output chunk 0 count;
            loop ())
  in
  loop ()

let proxy_status head =
  match String.split_on_char '\n' head with
  | status_line :: _ -> (
      match String.split_on_char ' ' (String.trim status_line) with
      | protocol :: status :: _ when String.starts_with ~prefix:"HTTP/" protocol
        ->
          Option.to_result
            ~none:("invalid proxy response status: " ^ status)
            (int_of_string_opt status)
      | _ -> Error ("invalid proxy response status line: " ^ status_line))
  | [] -> Error "proxy returned an empty response"

let http_connect_proxy fd proxy ~host ~port =
  let authority = endpoint_authority host port in
  let output = Buffer.create 256 in
  Buffer.add_string output ("CONNECT " ^ authority ^ " HTTP/1.1\r\n");
  Buffer.add_string output ("Host: " ^ authority ^ "\r\n");
  Buffer.add_string output "Proxy-Connection: keep-alive\r\n";
  Option.iter
    (fun value ->
      Buffer.add_string output ("Proxy-Authorization: " ^ value ^ "\r\n"))
    (proxy_authorization proxy);
  Buffer.add_string output "\r\n";
  write_plain fd (Buffer.contents output) 0;
  let* head, pending = read_proxy_response_head fd in
  let* status = proxy_status head in
  if status >= 200 && status < 300 && pending <> "" then
    Error "proxy sent unexpected bytes after CONNECT response headers"
  else if status >= 200 && status < 300 then Ok ()
  else Error (Printf.sprintf "HTTP proxy CONNECT failed with status %d" status)

let socks5_reply_error = function
  | 1 -> "general SOCKS server failure"
  | 2 -> "SOCKS connection not allowed"
  | 3 -> "SOCKS network unreachable"
  | 4 -> "SOCKS host unreachable"
  | 5 -> "SOCKS connection refused"
  | 6 -> "SOCKS TTL expired"
  | 7 -> "SOCKS command not supported"
  | 8 -> "SOCKS address type not supported"
  | code -> Printf.sprintf "unknown SOCKS error %d" code

let socks5_proxy fd proxy ~host ~port =
  let credentials = proxy_credentials proxy in
  let greeting =
    match credentials with
    | None -> "\005\001\000"
    | Some _ -> "\005\002\000\002"
  in
  write_plain fd greeting 0;
  let* selection = read_exact_fd fd 2 in
  if Char.code selection.[0] <> 5 then Error "invalid SOCKS proxy version"
  else
    let* () =
      match (Char.code selection.[1], credentials) with
      | 0, _ -> Ok ()
      | 2, Some (user, password) ->
          if String.length user > 255 || String.length password > 255 then
            Error "SOCKS proxy username and password must be at most 255 bytes"
          else
            let auth =
              Buffer.create (3 + String.length user + String.length password)
            in
            Buffer.add_char auth '\001';
            Buffer.add_char auth (Char.chr (String.length user));
            Buffer.add_string auth user;
            Buffer.add_char auth (Char.chr (String.length password));
            Buffer.add_string auth password;
            write_plain fd (Buffer.contents auth) 0;
            let* response = read_exact_fd fd 2 in
            if Char.code response.[0] <> 1 then
              Error "invalid SOCKS authentication response version"
            else if Char.code response.[1] <> 0 then
              Error "SOCKS proxy authentication failed"
            else Ok ()
      | 2, None ->
          Error "SOCKS proxy requested credentials that were not configured"
      | 255, _ -> Error "SOCKS proxy rejected all authentication methods"
      | method_, _ ->
          Error
            (Printf.sprintf "SOCKS proxy selected unsupported method %d" method_)
    in
    if String.length host > 255 then
      Error "SOCKS destination host exceeds 255 bytes"
    else
      let request = Buffer.create (7 + String.length host) in
      Buffer.add_string request "\005\001\000\003";
      Buffer.add_char request (Char.chr (String.length host));
      Buffer.add_string request host;
      Buffer.add_char request (Char.chr ((port lsr 8) land 0xff));
      Buffer.add_char request (Char.chr (port land 0xff));
      write_plain fd (Buffer.contents request) 0;
      let* response = read_exact_fd fd 4 in
      if Char.code response.[0] <> 5 then Error "invalid SOCKS response version"
      else if Char.code response.[1] <> 0 then
        Error (socks5_reply_error (Char.code response.[1]))
      else
        let* address_length =
          match Char.code response.[3] with
          | 1 -> Ok 4
          | 4 -> Ok 16
          | 3 ->
              let* length = read_exact_fd fd 1 in
              Ok (Char.code length.[0])
          | value ->
              Error (Printf.sprintf "invalid SOCKS address type %d" value)
        in
        let* _address = read_exact_fd fd address_length in
        let* _port = read_exact_fd fd 2 in
        Ok ()

let with_interruptible_fd cancel fd fn =
  let lock = Mutex.create () in
  let interruptible = ref true in
  let interrupt () =
    Mutex.lock lock;
    (if !interruptible then
       try Unix.shutdown fd Unix.SHUTDOWN_ALL with Unix.Unix_error _ -> ());
    Mutex.unlock lock
  in
  let unregister = Cancel.on_cancel cancel interrupt in
  Fun.protect
    ~finally:(fun () ->
      unregister ();
      Mutex.lock lock;
      interruptible := false;
      Mutex.unlock lock)
    fn

let connect_via_proxy cancel proxy ~host ~port ~server_scheme =
  let scheme =
    Option.value ~default:""
      (Option.map String.lowercase_ascii (Uri.scheme proxy))
  in
  let* proxy_host =
    Option.to_result ~none:"proxy URL has no host" (Uri.host proxy)
  in
  let proxy_port =
    Option.value
      ~default:
        (match scheme with
        | "http" -> 80
        | "https" -> 443
        | _ -> 1080)
      (Uri.port proxy)
  in
  if scheme = "https" then
    Error
      "HTTPS proxy URLs are not supported by the native transport; use an HTTP \
       or SOCKS5 proxy"
  else
    let* fd = connect cancel proxy_host proxy_port in
    let finish result =
      match result with
      | Ok () -> Ok fd
      | Error _ as error ->
          (try Unix.close fd with Unix.Unix_error _ -> ());
          error
    in
    try
      with_interruptible_fd cancel fd (fun () ->
          match scheme with
          | "http" when server_scheme = "http" -> Ok fd
          | "http" -> finish (http_connect_proxy fd proxy ~host ~port)
          | "socks5" -> finish (socks5_proxy fd proxy ~host ~port)
          | unsupported ->
              (try Unix.close fd with Unix.Unix_error _ -> ());
              Error ("unsupported proxy URL scheme: " ^ unsupported))
    with exn ->
      (try Unix.close fd with Unix.Unix_error _ -> ());
      Error ("proxy handshake failed: " ^ Printexc.to_string exn)

let decode_certificates pem =
  match X509.Certificate.decode_pem_multiple pem with
  | Ok [] -> Error "certificate PEM contains no certificates"
  | Ok certificates -> Ok certificates
  | Error (`Msg message) -> Error message

let tls_authenticator config =
  if config.Config.insecure_skip_verify then
    Ok (fun ?ip:_ ~host:_ _certificates -> Ok None)
  else
    match config.ca_pem with
    | Some pem ->
        let* anchors = decode_certificates pem in
        Ok
          (X509.Authenticator.chain_of_trust
             ~time:(fun () -> Some (Ptime_clock.now ()))
             anchors)
    | None -> (
        match Ca_certs.authenticator () with
        | Ok authenticator -> Ok authenticator
        | Error (`Msg message) -> Error message)

let tls_client_config tls host =
  let* authenticator = tls_authenticator tls in
  let* certificates =
    match (tls.Config.client_certificate_pem, tls.client_key_pem) with
    | None, None -> Ok None
    | Some certificate_pem, Some key_pem ->
        let* certificates = decode_certificates certificate_pem in
        let* key =
          match X509.Private_key.decode_pem key_pem with
          | Ok key -> Ok key
          | Error (`Msg message) -> Error message
        in
        Ok (Some (`Single (certificates, key)))
    | _ -> Error "both client certificate and client key are required"
  in
  let verification_name = Option.value ~default:host tls.server_name in
  let peer_name, ip =
    match Ipaddr.of_string verification_name with
    | Ok ip -> (None, Some ip)
    | Error _ -> (
        match Domain_name.of_string verification_name with
        | Ok name -> (
            match Domain_name.host name with
            | Ok host -> (Some host, None)
            | Error _ -> (None, None))
        | Error _ -> (None, None))
  in
  match Tls.Config.client ~authenticator ?peer_name ?certificates ?ip () with
  | Ok value -> Ok (value, peer_name, ip)
  | Error (`Msg message) -> Error message

let open_flow_with cancel config =
  let* host =
    match Uri.host config.Config.server with
    | Some value -> Ok value
    | None -> Error "API server URL has no host"
  in
  let scheme = Option.value ~default:"https" (Uri.scheme config.server) in
  let port =
    Option.value
      ~default:(if scheme = "https" then 443 else 80)
      (Uri.port config.server)
  in
  let* fd =
    match config.Config.proxy_url with
    | None -> connect cancel host port
    | Some proxy ->
        connect_via_proxy cancel proxy ~host ~port ~server_scheme:scheme
  in
  if Cancel.is_cancelled cancel then (
    Unix.close fd;
    Error "request cancelled during connection establishment")
  else
    match scheme with
    | "http" -> Ok (Plain fd)
    | "https" -> (
        Crypto_runtime.ensure_rng ();
        match tls_client_config config.tls host with
        | Error message ->
            Unix.close fd;
            Error message
        | Ok (tls_config, peer_name, ip) ->
            with_interruptible_fd cancel fd (fun () ->
                try
                  Ok
                    (Tls
                       (Tls_unix.client_of_fd tls_config ?host:peer_name ?ip fd))
                with exn ->
                  (try Unix.close fd with Unix.Unix_error _ -> ());
                  Error (Printexc.to_string exn)))
    | unsupported ->
        Unix.close fd;
        Error ("unsupported API server URL scheme: " ^ unsupported)

let open_flow ?cancel ~connect_timeout config =
  let parent = Option.value ~default:(Cancel.create ()) cancel in
  let result, timed_out =
    Cancel.with_timeout ~parent connect_timeout (fun connect_cancel ->
        open_flow_with connect_cancel config)
  in
  let externally_cancelled =
    Option.fold ~none:false ~some:Cancel.is_cancelled cancel
  in
  if timed_out || externally_cancelled then (
    (match result with
    | Ok flow -> close_flow flow
    | Error _ -> ());
    if timed_out then
      Error
        (Printf.sprintf "connection establishment timed out after %.3fs"
           connect_timeout)
    else Error "request cancelled during connection establishment")
  else result

let header name headers =
  let name = String.lowercase_ascii name in
  List.find_map
    (fun (key, value) ->
      if String.lowercase_ascii key = name then Some value else None)
    headers

let transfer_is_chunked headers =
  match header "transfer-encoding" headers with
  | None -> false
  | Some value ->
      String.split_on_char ',' value
      |> List.exists (fun token ->
          String.lowercase_ascii (String.trim token) = "chunked")

let valid_header_name value =
  let is_token_character = function
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
  in
  value <> "" && String.for_all is_token_character value

let valid_header_value value =
  String.for_all
    (fun character ->
      let code = Char.code character in
      character = '\t' || (code >= 0x20 && code <> 0x7f))
    value

let valid_request_target value =
  value <> ""
  && value.[0] = '/'
  && String.for_all
       (fun character ->
         let code = Char.code character in
         code > 0x20 && code <> 0x7f)
       value

let header_is name candidate =
  String.lowercase_ascii candidate = String.lowercase_ascii name

let reserved_request_header name =
  List.exists
    (fun reserved -> header_is reserved name)
    [ "host"; "content-length"; "transfer-encoding"; "connection" ]

let split_once separator value =
  match String.index_opt value separator with
  | None -> (value, "")
  | Some index ->
      ( String.sub value 0 index,
        String.sub value (index + 1) (String.length value - index - 1) )

let parse_head value =
  match String.split_on_char '\n' value with
  | [] -> Error "empty HTTP response"
  | status_line :: raw_headers -> (
      let status_line = String.trim status_line in
      let parts = String.split_on_char ' ' status_line in
      match parts with
      | protocol :: status :: reason_parts
        when String.starts_with ~prefix:"HTTP/" protocol -> (
          match int_of_string_opt status with
          | None -> Error ("invalid HTTP status: " ^ status)
          | Some status ->
              let headers =
                List.filter_map
                  (fun line ->
                    let line = String.trim line in
                    if line = "" then None
                    else
                      let name, value = split_once ':' line in
                      Some
                        ( String.lowercase_ascii (String.trim name),
                          String.trim value ))
                  raw_headers
              in
              Ok (protocol, status, String.concat " " reason_parts, headers))
      | _ -> Error ("invalid HTTP status line: " ^ status_line))

let read_response_head ?cancel ~timeout flow =
  let head_buffer = Buffer.create 4096 in
  let read_buffer = Bytes.create 4096 in
  let rec read_head () =
    if Buffer.length head_buffer > 1024 * 1024 then
      Error "HTTP headers exceed 1 MiB"
    else
      let contents = Buffer.contents head_buffer in
      let marker = "\r\n\r\n" in
      let rec find index =
        if index + 4 > String.length contents then None
        else if String.sub contents index 4 = marker then Some index
        else find (index + 1)
      in
      match find 0 with
      | Some boundary ->
          Ok
            ( String.sub contents 0 boundary,
              String.sub contents (boundary + 4)
                (String.length contents - boundary - 4) )
      | None ->
          let count = read flow read_buffer 0 (Bytes.length read_buffer) in
          if count = 0 then Error "EOF before HTTP response headers"
          else (
            Buffer.add_subbytes head_buffer read_buffer 0 count;
            read_head ())
  in
  with_phase_timeout ?cancel ~phase:"response headers" ~timeout flow read_head

type input = { flow : flow; pending : string; mutable offset : int }

let input_read input buffer off len =
  let available = String.length input.pending - input.offset in
  if available > 0 then (
    let count = min available len in
    Bytes.blit_string input.pending input.offset buffer off count;
    input.offset <- input.offset + count;
    count)
  else read input.flow buffer off len

let read_exact input length consume =
  let buffer = Bytes.create (min 16384 (max 1 length)) in
  let rec loop remaining =
    if remaining = 0 then Ok ()
    else
      let requested = min remaining (Bytes.length buffer) in
      let count = input_read input buffer 0 requested in
      if count = 0 then Error "unexpected EOF in HTTP body"
      else (
        consume (Bytes.sub_string buffer 0 count);
        loop (remaining - count))
  in
  loop length

let read_to_eof input consume =
  let buffer = Bytes.create 16384 in
  let rec loop () =
    let count = input_read input buffer 0 (Bytes.length buffer) in
    if count = 0 then Ok ()
    else (
      consume (Bytes.sub_string buffer 0 count);
      loop ())
  in
  loop ()

let read_line input =
  let output = Buffer.create 32 in
  let one = Bytes.create 1 in
  let rec loop previous_cr =
    let count = input_read input one 0 1 in
    if count = 0 then Error "unexpected EOF while reading HTTP line"
    else
      let char = Bytes.get one 0 in
      if previous_cr && char = '\n' then
        let value = Buffer.contents output in
        Ok (String.sub value 0 (String.length value - 1))
      else (
        Buffer.add_char output char;
        loop (char = '\r'))
  in
  loop false

let read_chunked input consume =
  let rec loop () =
    let* line = read_line input in
    let size_text, _extensions = split_once ';' line in
    match int_of_string_opt ("0x" ^ String.trim size_text) with
    | None -> Error ("invalid HTTP chunk size: " ^ line)
    | Some 0 ->
        let rec trailers () =
          let* line = read_line input in
          if line = "" then Ok () else trailers ()
        in
        trailers ()
    | Some size ->
        let* () = read_exact input size consume in
        let* terminator = read_line input in
        if terminator <> "" then Error "invalid HTTP chunk terminator"
        else loop ()
  in
  loop ()

let target config path =
  let prefix = Uri.path config.Config.server in
  match (prefix, path) with
  | ("" | "/"), path -> path
  | prefix, path
    when String.ends_with ~suffix:"/" prefix
         && String.starts_with ~prefix:"/" path ->
      prefix ^ String.sub path 1 (String.length path - 1)
  | prefix, path
    when (not (String.ends_with ~suffix:"/" prefix))
         && not (String.starts_with ~prefix:"/" path) -> prefix ^ "/" ^ path
  | prefix, path -> prefix ^ path

let uses_forward_http_proxy config =
  match config.Config.proxy_url with
  | Some proxy ->
      Option.map String.lowercase_ascii (Uri.scheme proxy) = Some "http"
      && Option.map String.lowercase_ascii (Uri.scheme config.server)
         = Some "http"
  | None -> false

let request_target config path =
  let origin_target = target config path in
  if uses_forward_http_proxy config then
    let host = Option.value ~default:"" (Uri.host config.Config.server) in
    let port = Option.value ~default:80 (Uri.port config.server) in
    let authority =
      if port = 80 then
        if String.contains host ':' then "[" ^ host ^ "]" else host
      else endpoint_authority host port
    in
    "http://" ^ authority ^ origin_target
  else origin_target

let checkout ?cancel transport =
  let rec take () =
    Mutex.lock transport.lock;
    match (transport.closed, transport.idle) with
    | true, _ ->
        Mutex.unlock transport.lock;
        Error "HTTP transport is closed"
    | false, flow :: rest ->
        transport.idle <- rest;
        Mutex.unlock transport.lock;
        if flow_is_idle flow then Ok flow
        else (
          close_flow flow;
          take ())
    | false, [] ->
        Mutex.unlock transport.lock;
        open_flow ?cancel ~connect_timeout:transport.connect_timeout
          transport.config
  in
  take ()

let checkin transport ~reusable flow =
  Mutex.lock transport.lock;
  let keep =
    reusable && (not transport.closed)
    && List.length transport.idle < transport.max_idle_connections
  in
  if keep then transport.idle <- flow :: transport.idle;
  Mutex.unlock transport.lock;
  if not keep then close_flow flow

let connection_has_token token headers =
  match header "connection" headers with
  | None -> false
  | Some value ->
      String.split_on_char ',' value
      |> List.exists (fun value ->
          String.lowercase_ascii (String.trim value) = token)

let connection_is_persistent protocol headers =
  if protocol = "HTTP/1.1" then not (connection_has_token "close" headers)
  else connection_has_token "keep-alive" headers

let request_with ?cancel ?(headers = []) ?(body = "") ?on_chunk
    ?(max_body_bytes = 32 * 1024 * 1024) ~(config : Config.t) ~acquire ~release
    ~persistent ~write_timeout ~response_header_timeout meth path =
  let cancelled () =
    match cancel with
    | Some token -> Cancel.is_cancelled token
    | None -> false
  in
  if max_body_bytes < 0 then Error "max_body_bytes must not be negative"
  else if not (valid_request_target path) then
    Error "request path must be an encoded absolute path without whitespace"
  else if
    List.exists
      (fun (name, value) ->
        (not (valid_header_name name)) || not (valid_header_value value))
      headers
  then Error "request header contains an invalid name or value"
  else if List.exists (fun (name, _) -> reserved_request_header name) headers
  then
    Error
      "Host, Content-Length, Transfer-Encoding, and Connection are managed by \
       the transport"
  else if cancelled () then Error "request cancelled"
  else
    match acquire () with
    | Error _ as error -> error
    | Ok flow ->
        let fd = flow_fd flow in
        let reusable = ref false in
        let unregister =
          match cancel with
          | None -> fun () -> ()
          | Some cancel ->
              Cancel.on_cancel cancel (fun () ->
                  try Unix.shutdown fd Unix.SHUTDOWN_ALL
                  with Unix.Unix_error _ -> ())
        in
        Fun.protect
          ~finally:(fun () ->
            unregister ();
            release ~reusable:(!reusable && not (cancelled ())) flow)
          (fun () ->
            let result =
              try
                let host = Option.value ~default:"" (Uri.host config.server) in
                let host_for_header =
                  if String.contains host ':' then "[" ^ host ^ "]" else host
                in
                let scheme =
                  Option.value ~default:"https" (Uri.scheme config.server)
                in
                let default_port = if scheme = "https" then 443 else 80 in
                let host_header =
                  match Uri.port config.server with
                  | Some port when port <> default_port ->
                      host_for_header ^ ":" ^ string_of_int port
                  | _ -> host_for_header
                in
                let default_header name value =
                  if
                    List.exists
                      (fun (candidate, _) -> header_is name candidate)
                      headers
                  then []
                  else [ (name, value) ]
                in
                let request_headers =
                  [
                    ("Host", host_header);
                    ("Connection", if persistent then "keep-alive" else "close");
                    ("Content-Length", string_of_int (String.length body));
                  ]
                  @ default_header "Accept" "application/json"
                  @ default_header "Accept-Encoding" "identity"
                  @ default_header "User-Agent" "ocaml-k8s"
                  @ (if uses_forward_http_proxy config then
                       match config.proxy_url with
                       | Some proxy ->
                           Option.fold ~none:[]
                             ~some:(fun value ->
                               default_header "Proxy-Authorization" value)
                             (proxy_authorization proxy)
                       | None -> []
                     else [])
                  @ headers
                in
                let output = Buffer.create (512 + String.length body) in
                Buffer.add_string output
                  (method_string meth ^ " " ^ request_target config path
                 ^ " HTTP/1.1\r\n");
                List.iter
                  (fun (name, value) ->
                    Buffer.add_string output (name ^ ": " ^ value ^ "\r\n"))
                  request_headers;
                Buffer.add_string output "\r\n";
                Buffer.add_string output body;
                with_phase_timeout ?cancel ~phase:"request write"
                  ~timeout:write_timeout flow (fun () ->
                    write flow (Buffer.contents output));
                let* head, pending =
                  read_response_head ?cancel ~timeout:response_header_timeout
                    flow
                in
                let* protocol, status, reason, response_headers =
                  parse_head head
                in
                let body_buffer = Buffer.create 4096 in
                let buffered_bytes = ref 0 in
                let consume value =
                  if on_chunk = None || status < 200 || status >= 300 then (
                    buffered_bytes := !buffered_bytes + String.length value;
                    if !buffered_bytes > max_body_bytes then
                      raise (Body_too_large max_body_bytes);
                    Buffer.add_string body_buffer value);
                  if status >= 200 && status < 300 then
                    Option.iter (fun callback -> callback value) on_chunk
                in
                let input = { flow; pending; offset = 0 } in
                let no_body =
                  (status >= 100 && status < 200)
                  || status = 204 || status = 304
                in
                let* body_is_framed =
                  if no_body then Ok true
                  else if transfer_is_chunked response_headers then
                    let* () = read_chunked input consume in
                    Ok true
                  else
                    match header "content-length" response_headers with
                    | Some value -> (
                        match int_of_string_opt value with
                        | Some length when length >= 0 ->
                            if on_chunk = None && length > max_body_bytes then
                              Error
                                (Printf.sprintf "HTTP body exceeds %d bytes"
                                   max_body_bytes)
                            else
                              let* () = read_exact input length consume in
                              Ok true
                        | _ -> Error ("invalid Content-Length: " ^ value))
                    | None ->
                        let* () = read_to_eof input consume in
                        Ok false
                in
                reusable :=
                  persistent && body_is_framed
                  && connection_is_persistent protocol response_headers;
                Ok
                  {
                    status;
                    reason;
                    headers = response_headers;
                    body = Buffer.contents body_buffer;
                  }
              with
              | Unix.Unix_error (code, fn, argument) ->
                  if cancelled () then Error "request cancelled"
                  else
                    Error
                      (Printf.sprintf "%s(%s): %s" fn argument
                         (Unix.error_message code))
              | Tls_unix.Tls_alert _ ->
                  Error "TLS alert received from API server"
              | Tls_unix.Tls_failure failure ->
                  Error
                    (Format.asprintf "TLS failure: %a" Tls.Engine.pp_failure
                       failure)
              | Tls_unix.Closed_by_peer | End_of_file ->
                  Error "connection closed by peer"
              | Body_too_large limit ->
                  Error (Printf.sprintf "HTTP body exceeds %d bytes" limit)
              | Request_phase_timeout (phase, timeout) ->
                  Error
                    (Printf.sprintf "%s timed out after %.3fs" phase timeout)
              | exn -> Error (Printexc.to_string exn)
            in
            match result with
            | Error _ when cancelled () -> Error "request cancelled"
            | result -> result)

let request ?cancel ?headers ?body ?on_chunk ?max_body_bytes transport meth path
    =
  request_with ?cancel ?headers ?body ?on_chunk ?max_body_bytes
    ~config:transport.config
    ~acquire:(fun () -> checkout ?cancel transport)
    ~release:(checkin transport) ~persistent:true
    ~write_timeout:transport.write_timeout
    ~response_header_timeout:transport.response_header_timeout meth path

let request_once ?cancel ?headers ?body ?on_chunk ?max_body_bytes
    ?(connect_timeout = 10.) ?(write_timeout = 30.)
    ?(response_header_timeout = 30.) config meth path =
  if not (valid_timeout connect_timeout) then
    invalid_arg "Http.request_once: connect_timeout must be finite and positive";
  if not (valid_timeout write_timeout) then
    invalid_arg "Http.request_once: write_timeout must be finite and positive";
  if not (valid_timeout response_header_timeout) then
    invalid_arg
      "Http.request_once: response_header_timeout must be finite and positive";
  request_with ?cancel ?headers ?body ?on_chunk ?max_body_bytes ~config
    ~acquire:(fun () -> open_flow ?cancel ~connect_timeout config)
    ~release:(fun ~reusable:_ flow -> close_flow flow)
    ~persistent:false ~write_timeout ~response_header_timeout meth path

type upgrade_result =
  | Upgraded of { response : response; connection : Upgrade.t }
  | Response of response

let upgrade_once ?cancel ?(headers = []) ?(max_body_bytes = 1024 * 1024)
    ?(connect_timeout = 10.) ?(write_timeout = 30.)
    ?(response_header_timeout = 30.) (config : Config.t) path =
  let cancelled () = Option.fold ~none:false ~some:Cancel.is_cancelled cancel in
  if max_body_bytes < 0 then Error "max_body_bytes must not be negative"
  else if not (valid_timeout connect_timeout) then
    Error "connect_timeout must be finite and positive"
  else if not (valid_timeout write_timeout) then
    Error "write_timeout must be finite and positive"
  else if not (valid_timeout response_header_timeout) then
    Error "response_header_timeout must be finite and positive"
  else if not (valid_request_target path) then
    Error "request path must be an encoded absolute path without whitespace"
  else if
    List.exists
      (fun (name, value) ->
        (not (valid_header_name name)) || not (valid_header_value value))
      headers
  then Error "request header contains an invalid name or value"
  else if
    List.exists
      (fun (name, _) ->
        List.exists (header_is name)
          [ "host"; "content-length"; "transfer-encoding"; "connection" ])
      headers
  then
    Error
      "Host, Content-Length, Transfer-Encoding, and Connection are managed by \
       the transport"
  else if not (List.exists (fun (name, _) -> header_is "upgrade" name) headers)
  then Error "an Upgrade request header is required"
  else if cancelled () then Error "request cancelled"
  else
    let* flow = open_flow ?cancel ~connect_timeout config in
    let transferred = ref false in
    Fun.protect
      ~finally:(fun () -> if not !transferred then close_flow flow)
      (fun () ->
        try
          let host = Option.value ~default:"" (Uri.host config.server) in
          let host_for_header =
            if String.contains host ':' then "[" ^ host ^ "]" else host
          in
          let scheme =
            Option.value ~default:"https" (Uri.scheme config.server)
          in
          let default_port = if scheme = "https" then 443 else 80 in
          let host_header =
            match Uri.port config.server with
            | Some port when port <> default_port ->
                host_for_header ^ ":" ^ string_of_int port
            | _ -> host_for_header
          in
          let default_header name value =
            if
              List.exists
                (fun (candidate, _) -> header_is name candidate)
                headers
            then []
            else [ (name, value) ]
          in
          let request_headers =
            [
              ("Host", host_header);
              ("Connection", "Upgrade");
              ("Content-Length", "0");
            ]
            @ default_header "User-Agent" "ocaml-k8s"
            @ (if uses_forward_http_proxy config then
                 match config.proxy_url with
                 | Some proxy ->
                     Option.fold ~none:[]
                       ~some:(fun value ->
                         default_header "Proxy-Authorization" value)
                       (proxy_authorization proxy)
                 | None -> []
               else [])
            @ headers
          in
          let output = Buffer.create 512 in
          Buffer.add_string output
            ("GET " ^ request_target config path ^ " HTTP/1.1\r\n");
          List.iter
            (fun (name, value) ->
              Buffer.add_string output (name ^ ": " ^ value ^ "\r\n"))
            request_headers;
          Buffer.add_string output "\r\n";
          with_phase_timeout ?cancel ~phase:"request write"
            ~timeout:write_timeout flow (fun () ->
              write flow (Buffer.contents output));
          let* head, pending =
            read_response_head ?cancel ~timeout:response_header_timeout flow
          in
          let* _protocol, status, reason, response_headers = parse_head head in
          if status = 101 then
            if not (connection_has_token "upgrade" response_headers) then
              Error "HTTP 101 response is missing Connection: Upgrade"
            else if header "upgrade" response_headers = None then
              Error "HTTP 101 response is missing the Upgrade header"
            else
              let response =
                { status; reason; headers = response_headers; body = "" }
              in
              let connection = Upgrade.create ?cancel flow pending in
              if cancelled () then (
                Upgrade.close connection;
                Error "request cancelled")
              else (
                transferred := true;
                Ok (Upgraded { response; connection }))
          else
            let body_buffer = Buffer.create (min 4096 max_body_bytes) in
            let buffered_bytes = ref 0 in
            let consume value =
              buffered_bytes := !buffered_bytes + String.length value;
              if !buffered_bytes > max_body_bytes then
                raise (Body_too_large max_body_bytes);
              Buffer.add_string body_buffer value
            in
            let input = { flow; pending; offset = 0 } in
            let no_body =
              (status >= 100 && status < 200) || status = 204 || status = 304
            in
            let* () =
              if no_body then Ok ()
              else if transfer_is_chunked response_headers then
                read_chunked input consume
              else
                match header "content-length" response_headers with
                | Some value -> (
                    match int_of_string_opt value with
                    | Some length when length >= 0 ->
                        if length > max_body_bytes then
                          Error
                            (Printf.sprintf "HTTP body exceeds %d bytes"
                               max_body_bytes)
                        else read_exact input length consume
                    | _ -> Error ("invalid Content-Length: " ^ value))
                | None -> read_to_eof input consume
            in
            Ok
              (Response
                 {
                   status;
                   reason;
                   headers = response_headers;
                   body = Buffer.contents body_buffer;
                 })
        with
        | Unix.Unix_error (code, name, argument) ->
            if cancelled () then Error "request cancelled"
            else
              Error
                (Printf.sprintf "%s(%s): %s" name argument
                   (Unix.error_message code))
        | Tls_unix.Tls_alert _ -> Error "TLS alert received from API server"
        | Tls_unix.Tls_failure failure ->
            Error
              (Format.asprintf "TLS failure: %a" Tls.Engine.pp_failure failure)
        | Tls_unix.Closed_by_peer | End_of_file ->
            Error "connection closed by peer"
        | Body_too_large limit ->
            Error (Printf.sprintf "HTTP body exceeds %d bytes" limit)
        | Request_phase_timeout (phase, timeout) ->
            Error (Printf.sprintf "%s timed out after %.3fs" phase timeout)
        | exn -> Error (Printexc.to_string exn))
