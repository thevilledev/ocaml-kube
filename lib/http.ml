type meth = [ `GET | `POST | `PUT | `PATCH | `DELETE ]

type response = {
  status : int;
  reason : string;
  headers : (string * string) list;
  body : string;
}

let () = Sys.set_signal Sys.sigpipe Sys.Signal_ignore

type flow = Plain of Unix.file_descr | Tls of Tls_unix.t

exception Body_too_large of int

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

let close = function
  | Plain fd -> ( try Unix.close fd with Unix.Unix_error _ -> ())
  | Tls tls -> ( try Tls_unix.close tls with _ -> ())

let connect host port =
  try
    let addresses =
      Unix.getaddrinfo host (string_of_int port)
        [ Unix.AI_SOCKTYPE Unix.SOCK_STREAM ]
    in
    let rec attempt errors = function
      | [] ->
          Error
            (match errors with
            | [] -> "no address found for " ^ host
            | message :: _ -> message)
      | address :: rest -> (
          let fd =
            Unix.socket address.Unix.ai_family address.ai_socktype
              address.ai_protocol
          in
          try
            Unix.set_close_on_exec fd;
            Unix.connect fd address.ai_addr;
            (try Unix.setsockopt fd Unix.TCP_NODELAY true
             with Unix.Unix_error _ -> ());
            Ok fd
          with Unix.Unix_error (code, fn, argument) ->
            Unix.close fd;
            attempt
              (Printf.sprintf "%s(%s): %s" fn argument (Unix.error_message code)
              :: errors)
              rest)
    in
    attempt [] addresses
  with Unix.Unix_error (code, fn, argument) ->
    Error (Printf.sprintf "%s(%s): %s" fn argument (Unix.error_message code))

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

let rng_initialized = Atomic.make false

let ensure_rng () =
  if Atomic.compare_and_set rng_initialized false true then
    Mirage_crypto_rng_unix.use_default ()

let open_flow config =
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
  let* fd = connect host port in
  match scheme with
  | "http" -> Ok (Plain fd)
  | "https" -> (
      ensure_rng ();
      match tls_client_config config.tls host with
      | Error message ->
          Unix.close fd;
          Error message
      | Ok (tls_config, peer_name, ip) -> (
          try Ok (Tls (Tls_unix.client_of_fd tls_config ?host:peer_name ?ip fd))
          with exn ->
            (try Unix.close fd with Unix.Unix_error _ -> ());
            Error (Printexc.to_string exn)))
  | unsupported ->
      Unix.close fd;
      Error ("unsupported API server URL scheme: " ^ unsupported)

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
              Ok (status, String.concat " " reason_parts, headers))
      | _ -> Error ("invalid HTTP status line: " ^ status_line))

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

let request ?cancel ?(headers = []) ?(body = "") ?on_chunk
    ?(max_body_bytes = 32 * 1024 * 1024) config meth path =
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
    match open_flow config with
    | Error _ as error -> error
    | Ok flow ->
        let fd =
          match flow with
          | Plain fd -> fd
          | Tls tls -> Tls_unix.file_descr tls
        in
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
            close flow)
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
                  ("Connection", "close");
                  ("Content-Length", string_of_int (String.length body));
                ]
                @ default_header "Accept" "application/json"
                @ default_header "Accept-Encoding" "identity"
                @ default_header "User-Agent" "kube-ocaml"
                @ headers
              in
              let output = Buffer.create (512 + String.length body) in
              Buffer.add_string output
                (method_string meth ^ " " ^ target config path ^ " HTTP/1.1\r\n");
              List.iter
                (fun (name, value) ->
                  Buffer.add_string output (name ^ ": " ^ value ^ "\r\n"))
                request_headers;
              Buffer.add_string output "\r\n";
              Buffer.add_string output body;
              write flow (Buffer.contents output);
              let head_buffer = Buffer.create 4096 in
              let read_buffer = Bytes.create 4096 in
              let rec read_head () =
                if Buffer.length head_buffer > 1024 * 1024 then
                  Error "HTTP headers exceed 1 MiB"
                else
                  let contents = Buffer.contents head_buffer in
                  match String.index_opt contents '\r' with
                  | Some _ -> (
                      let marker = "\r\n\r\n" in
                      let rec find index =
                        if index + 4 > String.length contents then None
                        else if String.sub contents index 4 = marker then
                          Some index
                        else find (index + 1)
                      in
                      match find 0 with
                      | Some boundary ->
                          Ok
                            ( String.sub contents 0 boundary,
                              String.sub contents (boundary + 4)
                                (String.length contents - boundary - 4) )
                      | None ->
                          let count =
                            read flow read_buffer 0 (Bytes.length read_buffer)
                          in
                          if count = 0 then
                            Error "EOF before HTTP response headers"
                          else (
                            Buffer.add_subbytes head_buffer read_buffer 0 count;
                            read_head ()))
                  | None ->
                      let count =
                        read flow read_buffer 0 (Bytes.length read_buffer)
                      in
                      if count = 0 then Error "EOF before HTTP response headers"
                      else (
                        Buffer.add_subbytes head_buffer read_buffer 0 count;
                        read_head ())
              in
              let* head, pending = read_head () in
              let* status, reason, response_headers = parse_head head in
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
              let* () =
                if transfer_is_chunked response_headers then
                  read_chunked input consume
                else
                  match header "content-length" response_headers with
                  | Some value -> (
                      match int_of_string_opt value with
                      | Some length when length >= 0 ->
                          if on_chunk = None && length > max_body_bytes then
                            Error
                              (Printf.sprintf "HTTP body exceeds %d bytes"
                                 max_body_bytes)
                          else read_exact input length consume
                      | _ -> Error ("invalid Content-Length: " ^ value))
                  | None -> read_to_eof input consume
              in
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
            | Tls_unix.Tls_alert _ -> Error "TLS alert received from API server"
            | Tls_unix.Tls_failure failure ->
                Error
                  (Format.asprintf "TLS failure: %a" Tls.Engine.pp_failure
                     failure)
            | Tls_unix.Closed_by_peer | End_of_file ->
                Error "connection closed by peer"
            | Body_too_large limit ->
                Error (Printf.sprintf "HTTP body exceeds %d bytes" limit)
            | exn -> Error (Printexc.to_string exn))
