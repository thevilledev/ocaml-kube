type close = { code : int option; reason : string }
type message = Binary of string | Text of string | Close of close

type connect_error =
  | Transport of string
  | Http_response of Http.response
  | Protocol of string

let ( let* ) result fn =
  match result with
  | Ok value -> fn value
  | Error _ as error -> error

module Sha1 = struct
  let rotate_left value bits =
    Int32.logor
      (Int32.shift_left value bits)
      (Int32.shift_right_logical value (32 - bits))

  let get_u32 bytes offset =
    let byte index =
      Int32.of_int (Char.code (Bytes.get bytes (offset + index)))
    in
    Int32.logor
      (Int32.shift_left (byte 0) 24)
      (Int32.logor
         (Int32.shift_left (byte 1) 16)
         (Int32.logor (Int32.shift_left (byte 2) 8) (byte 3)))

  let set_u32 bytes offset value =
    for index = 0 to 3 do
      let shift = 24 - (index * 8) in
      Bytes.set bytes (offset + index)
        (Char.chr
           (Int32.to_int
              (Int32.logand (Int32.shift_right_logical value shift) 0xffl)))
    done

  let digest value =
    let length = String.length value in
    let padded_length = (length + 9 + 63) / 64 * 64 in
    let bytes = Bytes.make padded_length '\000' in
    Bytes.blit_string value 0 bytes 0 length;
    Bytes.set bytes length '\x80';
    let bit_length = Int64.mul (Int64.of_int length) 8L in
    for index = 0 to 7 do
      let shift = (7 - index) * 8 in
      Bytes.set bytes
        (padded_length - 8 + index)
        (Char.chr
           (Int64.to_int
              (Int64.logand (Int64.shift_right_logical bit_length shift) 0xffL)))
    done;
    let h0 = ref 0x67452301l in
    let h1 = ref 0xefcdab89l in
    let h2 = ref 0x98badcfel in
    let h3 = ref 0x10325476l in
    let h4 = ref 0xc3d2e1f0l in
    let words = Array.make 80 0l in
    for block = 0 to (padded_length / 64) - 1 do
      let offset = block * 64 in
      for index = 0 to 15 do
        words.(index) <- get_u32 bytes (offset + (index * 4))
      done;
      for index = 16 to 79 do
        words.(index) <-
          rotate_left
            (Int32.logxor
               words.(index - 3)
               (Int32.logxor
                  words.(index - 8)
                  (Int32.logxor words.(index - 14) words.(index - 16))))
            1
      done;
      let a = ref !h0 in
      let b = ref !h1 in
      let c = ref !h2 in
      let d = ref !h3 in
      let e = ref !h4 in
      for index = 0 to 79 do
        let f, k =
          if index < 20 then
            ( Int32.logor (Int32.logand !b !c)
                (Int32.logand (Int32.lognot !b) !d),
              0x5a827999l )
          else if index < 40 then
            (Int32.logxor !b (Int32.logxor !c !d), 0x6ed9eba1l)
          else if index < 60 then
            ( Int32.logor (Int32.logand !b !c)
                (Int32.logor (Int32.logand !b !d) (Int32.logand !c !d)),
              0x8f1bbcdcl )
          else (Int32.logxor !b (Int32.logxor !c !d), 0xca62c1d6l)
        in
        let temporary =
          Int32.add (rotate_left !a 5)
            (Int32.add f (Int32.add !e (Int32.add k words.(index))))
        in
        e := !d;
        d := !c;
        c := rotate_left !b 30;
        b := !a;
        a := temporary
      done;
      h0 := Int32.add !h0 !a;
      h1 := Int32.add !h1 !b;
      h2 := Int32.add !h2 !c;
      h3 := Int32.add !h3 !d;
      h4 := Int32.add !h4 !e
    done;
    let output = Bytes.create 20 in
    List.iteri
      (fun index word -> set_u32 output (index * 4) word)
      [ !h0; !h1; !h2; !h3; !h4 ];
    Bytes.unsafe_to_string output
end

let handshake_accept key =
  Base64.encode_string
    (Sha1.digest (key ^ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"))

module For_testing = struct
  let handshake_accept = handshake_accept
end

type t = {
  connection : Http.Upgrade.t;
  protocol : string option;
  max_message_bytes : int;
  close_sent : bool Atomic.t;
  receive_lock : Mutex.t;
}

let header name headers =
  let name = String.lowercase_ascii name in
  List.find_map
    (fun (candidate, value) ->
      if String.lowercase_ascii candidate = name then Some value else None)
    headers

let valid_token value =
  let valid = function
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
    (* Kubernetes' standardized port-forward protocol contains a slash even
       though RFC 6455 refers to the narrower HTTP token grammar. Keep the
       compatibility extension limited to this non-delimiting character. *)
    | '/'
    | '-'
    | '.'
    | '^'
    | '_'
    | '`'
    | '|'
    | '~' -> true
    | _ -> false
  in
  value <> "" && String.for_all valid value

let protocol connection = connection.protocol
let is_closed connection = Http.Upgrade.is_closed connection.connection

let random_bytes length =
  Crypto_runtime.ensure_rng ();
  Mirage_crypto_rng.generate length

let set_u16 bytes offset value =
  Bytes.set bytes offset (Char.chr ((value lsr 8) land 0xff));
  Bytes.set bytes (offset + 1) (Char.chr (value land 0xff))

let set_u64 bytes offset value =
  for index = 0 to 7 do
    let shift = (7 - index) * 8 in
    Bytes.set bytes (offset + index)
      (Char.chr
         (Int64.to_int
            (Int64.logand (Int64.shift_right_logical value shift) 0xffL)))
  done

let send_frame connection ~opcode payload =
  if Http.Upgrade.is_closed connection.connection then
    Error "WebSocket connection is closed"
  else if opcode >= 8 && String.length payload > 125 then
    Error "WebSocket control payload exceeds 125 bytes"
  else
    let length = String.length payload in
    let extended =
      if length <= 125 then 0 else if length <= 0xffff then 2 else 8
    in
    let header_length = 2 + extended + 4 in
    let frame = Bytes.create (header_length + length) in
    Bytes.set frame 0 (Char.chr (0x80 lor opcode));
    Bytes.set frame 1
      (Char.chr
         (0x80
         lor if extended = 0 then length else if extended = 2 then 126 else 127
         ));
    if extended = 2 then set_u16 frame 2 length
    else if extended = 8 then set_u64 frame 2 (Int64.of_int length);
    let mask_offset = 2 + extended in
    let mask = random_bytes 4 in
    Bytes.blit_string mask 0 frame mask_offset 4;
    for index = 0 to length - 1 do
      let masked =
        Char.code payload.[index] lxor Char.code mask.[index land 3]
      in
      Bytes.set frame (header_length + index) (Char.chr masked)
    done;
    Http.Upgrade.write connection.connection (Bytes.unsafe_to_string frame)

let send_binary connection payload = send_frame connection ~opcode:2 payload
let send_text connection payload = send_frame connection ~opcode:1 payload

let send_ping connection payload =
  if String.length payload > 125 then
    Error "WebSocket ping payload exceeds 125 bytes"
  else send_frame connection ~opcode:9 payload

let close_payload code reason =
  match code with
  | None -> reason
  | Some code ->
      let output = Bytes.create (2 + String.length reason) in
      set_u16 output 0 code;
      Bytes.blit_string reason 0 output 2 (String.length reason);
      Bytes.unsafe_to_string output

let send_close connection code reason =
  if Atomic.compare_and_set connection.close_sent false true then
    ignore (send_frame connection ~opcode:8 (close_payload code reason))

let close ?code ?(reason = "") connection =
  if String.length reason <= 123 then send_close connection code reason;
  Http.Upgrade.close connection.connection

let read_exact connection bytes offset length =
  let rec loop offset remaining =
    if remaining = 0 then Ok ()
    else
      let* count = Http.Upgrade.read connection bytes offset remaining in
      if count = 0 then Error "unexpected EOF in WebSocket frame"
      else loop (offset + count) (remaining - count)
  in
  loop offset length

let get_u16 bytes offset =
  (Char.code (Bytes.get bytes offset) lsl 8)
  lor Char.code (Bytes.get bytes (offset + 1))

let get_u64 bytes offset =
  let value = ref 0L in
  for index = 0 to 7 do
    value :=
      Int64.logor
        (Int64.shift_left !value 8)
        (Int64.of_int (Char.code (Bytes.get bytes (offset + index))))
  done;
  !value

type frame = { final : bool; opcode : int; payload : string }

let protocol_failure connection message =
  send_close connection (Some 1002) message;
  Http.Upgrade.close connection.connection;
  Error message

let read_frame connection =
  let header = Bytes.create 2 in
  let* () = read_exact connection.connection header 0 2 in
  let first = Char.code (Bytes.get header 0) in
  let second = Char.code (Bytes.get header 1) in
  let final = first land 0x80 <> 0 in
  let reserved = first land 0x70 in
  let opcode = first land 0x0f in
  let masked = second land 0x80 <> 0 in
  if reserved <> 0 then protocol_failure connection "WebSocket RSV bits are set"
  else if masked then
    protocol_failure connection "server WebSocket frames must not be masked"
  else if opcode >= 8 && not final then
    protocol_failure connection "fragmented WebSocket control frame"
  else
    let marker = second land 0x7f in
    let* length =
      if marker <= 125 then Ok (Int64.of_int marker)
      else if marker = 126 then
        let extended = Bytes.create 2 in
        let* () = read_exact connection.connection extended 0 2 in
        let value = get_u16 extended 0 in
        if value < 126 then Error "non-canonical WebSocket payload length"
        else Ok (Int64.of_int value)
      else
        let extended = Bytes.create 8 in
        let* () = read_exact connection.connection extended 0 8 in
        let value = get_u64 extended 0 in
        if Int64.compare value 0L < 0 then
          Error "WebSocket payload length has the reserved high bit set"
        else if Int64.compare value 65536L < 0 then
          Error "non-canonical WebSocket payload length"
        else Ok value
    in
    if opcode >= 8 && Int64.compare length 125L > 0 then
      protocol_failure connection "WebSocket control payload exceeds 125 bytes"
    else if Int64.compare length (Int64.of_int connection.max_message_bytes) > 0
    then
      protocol_failure connection "WebSocket message exceeds configured limit"
    else
      let length = Int64.to_int length in
      let payload = Bytes.create length in
      let* () = read_exact connection.connection payload 0 length in
      Ok { final; opcode; payload = Bytes.unsafe_to_string payload }

let parse_close payload =
  match String.length payload with
  | 0 -> Ok { code = None; reason = "" }
  | 1 -> Error "WebSocket close payload contains one byte"
  | _ ->
      let bytes = Bytes.unsafe_of_string payload in
      let code = get_u16 bytes 0 in
      let reason = String.sub payload 2 (String.length payload - 2) in
      Ok { code = Some code; reason }

let receive_unlocked connection =
  let fragments = Buffer.create 4096 in
  let fragmented_opcode = ref None in
  let rec loop () =
    let* frame = read_frame connection in
    match frame.opcode with
    | 8 -> (
        match parse_close frame.payload with
        | Error message -> protocol_failure connection message
        | Ok close ->
            send_close connection close.code close.reason;
            Http.Upgrade.close connection.connection;
            Ok (Close close))
    | 9 ->
        let* () = send_frame connection ~opcode:10 frame.payload in
        loop ()
    | 10 -> loop ()
    | 0 -> (
        match !fragmented_opcode with
        | None ->
            protocol_failure connection "unexpected WebSocket continuation"
        | Some opcode ->
            if
              Buffer.length fragments + String.length frame.payload
              > connection.max_message_bytes
            then
              protocol_failure connection
                "WebSocket message exceeds configured limit"
            else (
              Buffer.add_string fragments frame.payload;
              if frame.final then
                let payload = Buffer.contents fragments in
                Ok (if opcode = 1 then Text payload else Binary payload)
              else loop ()))
    | (1 | 2) as opcode ->
        if !fragmented_opcode <> None then
          protocol_failure connection
            "new WebSocket data frame during fragmented message"
        else if frame.final then
          Ok (if opcode = 1 then Text frame.payload else Binary frame.payload)
        else (
          fragmented_opcode := Some opcode;
          Buffer.add_string fragments frame.payload;
          loop ())
    | _ -> protocol_failure connection "unsupported WebSocket opcode"
  in
  loop ()

let receive connection =
  Mutex.lock connection.receive_lock;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock connection.receive_lock)
    (fun () -> receive_unlocked connection)

let unique values =
  let rec loop seen = function
    | [] -> true
    | value :: rest ->
        if List.mem value seen then false else loop (value :: seen) rest
  in
  loop [] values

let connect ?cancel ?(headers = []) ?(protocols = [])
    ?(max_message_bytes = 64 * 1024 * 1024)
    ?(max_error_body_bytes = 1024 * 1024) ?connect_timeout ?write_timeout
    ?response_header_timeout config path =
  if max_message_bytes <= 0 then
    Error (Protocol "max_message_bytes must be positive")
  else if List.exists (fun protocol -> not (valid_token protocol)) protocols
  then Error (Protocol "WebSocket subprotocol contains an invalid token")
  else if not (unique protocols) then
    Error (Protocol "WebSocket subprotocols must be unique and ordered")
  else
    let reserved =
      [
        "upgrade";
        "connection";
        "sec-websocket-key";
        "sec-websocket-version";
        "sec-websocket-protocol";
      ]
    in
    if
      List.exists
        (fun (name, _) -> List.mem (String.lowercase_ascii name) reserved)
        headers
    then Error (Protocol "WebSocket handshake header is managed by the client")
    else
      let key = Base64.encode_string (random_bytes 16) in
      let protocol_headers =
        match protocols with
        | [] -> []
        | values -> [ ("Sec-WebSocket-Protocol", String.concat ", " values) ]
      in
      let request_headers =
        [
          ("Upgrade", "websocket");
          ("Sec-WebSocket-Version", "13");
          ("Sec-WebSocket-Key", key);
        ]
        @ protocol_headers @ headers
      in
      match
        Http.upgrade_once ?cancel ~headers:request_headers
          ~max_body_bytes:max_error_body_bytes ?connect_timeout ?write_timeout
          ?response_header_timeout config path
      with
      | Error message -> Error (Transport message)
      | Ok (Http.Response response) -> Error (Http_response response)
      | Ok (Http.Upgraded { response; connection }) ->
          let fail message =
            Http.Upgrade.close connection;
            Error (Protocol message)
          in
          let upgrade = header "upgrade" response.headers in
          let accept = header "sec-websocket-accept" response.headers in
          let selected =
            Option.map String.trim
              (header "sec-websocket-protocol" response.headers)
          in
          if Option.map String.lowercase_ascii upgrade <> Some "websocket" then
            fail "server did not select the WebSocket upgrade"
          else if accept <> Some (handshake_accept key) then
            fail "invalid Sec-WebSocket-Accept response"
          else if
            Option.fold ~none:false
              ~some:(fun protocol -> not (List.mem protocol protocols))
              selected
          then fail "server selected an unrequested WebSocket subprotocol"
          else
            Ok
              {
                connection;
                protocol = selected;
                max_message_bytes;
                close_sent = Atomic.make false;
                receive_lock = Mutex.create ();
              }
