let ( let* ) result fn =
  match result with
  | Ok value -> fn value
  | Error _ as error -> error

let add_u16 output value =
  Buffer.add_char output (Char.chr ((value lsr 8) land 0xff));
  Buffer.add_char output (Char.chr (value land 0xff))

let add_u24 output value =
  Buffer.add_char output (Char.chr ((value lsr 16) land 0xff));
  Buffer.add_char output (Char.chr ((value lsr 8) land 0xff));
  Buffer.add_char output (Char.chr (value land 0xff))

let add_u32 output value =
  Buffer.add_char output (Char.chr ((value lsr 24) land 0xff));
  Buffer.add_char output (Char.chr ((value lsr 16) land 0xff));
  Buffer.add_char output (Char.chr ((value lsr 8) land 0xff));
  Buffer.add_char output (Char.chr (value land 0xff))

let get_u24 value offset =
  (Char.code value.[offset] lsl 16)
  lor (Char.code value.[offset + 1] lsl 8)
  lor Char.code value.[offset + 2]

let get_u32 value offset =
  (Char.code value.[offset] lsl 24)
  lor (Char.code value.[offset + 1] lsl 16)
  lor (Char.code value.[offset + 2] lsl 8)
  lor Char.code value.[offset + 3]

let dictionary_words =
  [
    "options";
    "head";
    "post";
    "put";
    "delete";
    "trace";
    "accept";
    "accept-charset";
    "accept-encoding";
    "accept-language";
    "accept-ranges";
    "age";
    "allow";
    "authorization";
    "cache-control";
    "connection";
    "content-base";
    "content-encoding";
    "content-language";
    "content-length";
    "content-location";
    "content-md5";
    "content-range";
    "content-type";
    "date";
    "etag";
    "expect";
    "expires";
    "from";
    "host";
    "if-match";
    "if-modified-since";
    "if-none-match";
    "if-range";
    "if-unmodified-since";
    "last-modified";
    "location";
    "max-forwards";
    "pragma";
    "proxy-authenticate";
    "proxy-authorization";
    "range";
    "referer";
    "retry-after";
    "server";
    "te";
    "trailer";
    "transfer-encoding";
    "upgrade";
    "user-agent";
    "vary";
    "via";
    "warning";
    "www-authenticate";
    "method";
    "get";
    "status";
    "200 OK";
    "version";
    "HTTP/1.1";
    "url";
    "public";
    "set-cookie";
    "keep-alive";
    "origin";
  ]

let dictionary_tail =
  "100101201202205206300302303304305306307402405406407408409410411412413414415416417502504505203 \
   Non-Authoritative Information204 No Content301 Moved Permanently400 Bad \
   Request401 Unauthorized403 Forbidden404 Not Found500 Internal Server \
   Error501 Not Implemented503 Service UnavailableJan Feb Mar Apr May Jun Jul \
   Aug Sept Oct Nov Dec 00:00:00 Mon, Tue, Wed, Thu, Fri, Sat, Sun, \
   GMTchunked,text/html,image/png,image/jpg,image/gif,application/xml,application/xhtml+xml,text/plain,text/javascript,publicprivatemax-age=gzip,deflate,sdchcharset=utf-8charset=iso-8859-1,utf-,*,enq=0."

let dictionary =
  let output = Buffer.create 1423 in
  List.iter
    (fun word ->
      add_u32 output (String.length word);
      Buffer.add_string output word)
    dictionary_words;
  Buffer.add_string output dictionary_tail;
  Buffer.contents output

let adler32 value =
  let modulus = 65521 in
  let first = ref 1 in
  let second = ref 0 in
  String.iter
    (fun character ->
      first := (!first + Char.code character) mod modulus;
      second := (!second + !first) mod modulus)
    value;
  Int32.logor (Int32.shift_left (Int32.of_int !second) 16) (Int32.of_int !first)

let () =
  if String.length dictionary <> 1423 || adler32 dictionary <> 0xe3c6a7c2l then
    invalid_arg "embedded SPDY/3 dictionary failed its integrity check"

type wire_frame =
  | Syn_reply of { stream_id : int; fin : bool }
  | Data of { stream_id : int; fin : bool; payload : string }
  | Reset of { stream_id : int; status : int }
  | Ping of int
  | Go_away of { last_stream_id : int; status : int }
  | Ignored

type t = {
  socket : Websocket.t;
  deflater : Spdy_zlib.deflater;
  inflater : Spdy_zlib.inflater;
  write_lock : Mutex.t;
  close_lock : Mutex.t;
  streams_lock : Mutex.t;
  streams : (int, stream) Hashtbl.t;
  mutable next_stream_id : int;
  mutable last_stream_id : int;
  max_stream_buffer_bytes : int;
  closed : bool Atomic.t;
  mutable close_reason : string;
  mutable tunnel_pending : string;
  mutable tunnel_offset : int;
  ping_cancel : Cancel.t;
  mutable reader_thread : Thread.t option;
  mutable ping_thread : Thread.t option;
}

and stream = {
  connection : t;
  stream_id : int;
  send_lock : Mutex.t;
  lock : Mutex.t;
  changed : Condition.t;
  queue : string Queue.t;
  mutable queued_bytes : int;
  mutable current : string option;
  mutable current_offset : int;
  mutable replied : bool;
  mutable local_fin : bool;
  mutable remote_fin : bool;
  mutable failure : string option;
  reset_sent : bool Atomic.t;
}

let identifier stream = stream.stream_id

let control_frame ~frame_type ~flags payload =
  let output = Buffer.create (8 + String.length payload) in
  add_u16 output 0x8003;
  add_u16 output frame_type;
  Buffer.add_char output (Char.chr flags);
  add_u24 output (String.length payload);
  Buffer.add_string output payload;
  Buffer.contents output

let data_frame ~stream_id ~fin payload =
  let output = Buffer.create (8 + String.length payload) in
  add_u32 output stream_id;
  Buffer.add_char output (if fin then '\001' else '\000');
  add_u24 output (String.length payload);
  Buffer.add_string output payload;
  Buffer.contents output

let encode_headers fields =
  let fields =
    List.map (fun (name, value) -> (String.lowercase_ascii name, value)) fields
    |> List.sort (fun (left, _) (right, _) -> String.compare left right)
  in
  let output = Buffer.create 128 in
  add_u32 output (List.length fields);
  List.iter
    (fun (name, value) ->
      add_u32 output (String.length name);
      Buffer.add_string output name;
      add_u32 output (String.length value);
      Buffer.add_string output value)
    fields;
  Buffer.contents output

let parse_headers value =
  let length = String.length value in
  let offset = ref 0 in
  let take_u32 () =
    if !offset > length - 4 then Error "truncated SPDY header block"
    else
      let result = get_u32 value !offset in
      offset := !offset + 4;
      if result < 0 then Error "SPDY header length exceeds OCaml bounds"
      else Ok result
  in
  let take count =
    if count < 0 || !offset > length - count then
      Error "truncated SPDY header block"
    else
      let result = String.sub value !offset count in
      offset := !offset + count;
      Ok result
  in
  let* count = take_u32 () in
  if count > 1000 then Error "SPDY header count exceeds 1000"
  else
    let rec loop remaining fields =
      if remaining = 0 then
        if !offset = length then Ok (List.rev fields)
        else Error "SPDY header block has trailing bytes"
      else
        let* name_length = take_u32 () in
        if name_length > 1024 * 1024 then Error "SPDY header name exceeds 1 MiB"
        else
          let* name = take name_length in
          let* value_length = take_u32 () in
          if value_length > 1024 * 1024 then
            Error "SPDY header value exceeds 1 MiB"
          else
            let* value = take value_length in
            if name <> String.lowercase_ascii name then
              Error "SPDY header name is not lowercase"
            else loop (remaining - 1) ((name, value) :: fields)
    in
    loop count []

let send_locked connection value =
  if Atomic.get connection.closed then Error connection.close_reason
  else Websocket.send_binary connection.socket value

let with_writer connection fn =
  Mutex.lock connection.write_lock;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock connection.write_lock)
    (fun () ->
      if Atomic.get connection.closed then Error connection.close_reason
      else
        try fn () with
        | Failure message -> Error message
        | exn -> Error (Printexc.to_string exn))

let send_syn_stream connection ~stream_id headers =
  with_writer connection (fun () ->
      let header_block = encode_headers headers in
      let compressed = Spdy_zlib.deflate connection.deflater header_block in
      let payload = Buffer.create (10 + String.length compressed) in
      add_u32 payload stream_id;
      add_u32 payload 0;
      Buffer.add_char payload '\000';
      Buffer.add_char payload '\000';
      Buffer.add_string payload compressed;
      send_locked connection
        (control_frame ~frame_type:1 ~flags:0 (Buffer.contents payload)))

let send_data connection ~stream_id ~fin payload =
  with_writer connection (fun () ->
      send_locked connection (data_frame ~stream_id ~fin payload))

let send_reset connection ~stream_id ~status =
  with_writer connection (fun () ->
      let payload = Buffer.create 8 in
      add_u32 payload stream_id;
      add_u32 payload status;
      send_locked connection
        (control_frame ~frame_type:3 ~flags:0 (Buffer.contents payload)))

let send_ping connection identifier =
  with_writer connection (fun () ->
      let payload = Buffer.create 4 in
      add_u32 payload identifier;
      send_locked connection
        (control_frame ~frame_type:6 ~flags:0 (Buffer.contents payload)))

let send_go_away connection =
  with_writer connection (fun () ->
      let payload = Buffer.create 8 in
      add_u32 payload connection.last_stream_id;
      add_u32 payload 0;
      send_locked connection
        (control_frame ~frame_type:7 ~flags:0 (Buffer.contents payload)))

let tunnel_read_exact connection length =
  let output = Bytes.create length in
  let rec fill output_offset =
    if output_offset = length then Ok (Bytes.unsafe_to_string output)
    else
      let available =
        String.length connection.tunnel_pending - connection.tunnel_offset
      in
      if available > 0 then (
        let count = min available (length - output_offset) in
        Bytes.blit_string connection.tunnel_pending connection.tunnel_offset
          output output_offset count;
        connection.tunnel_offset <- connection.tunnel_offset + count;
        fill (output_offset + count))
      else
        match Websocket.receive connection.socket with
        | Error message -> Error message
        | Ok (Websocket.Binary "") -> fill output_offset
        | Ok (Websocket.Binary value) ->
            connection.tunnel_pending <- value;
            connection.tunnel_offset <- 0;
            fill output_offset
        | Ok (Websocket.Text _) -> Error "SPDY tunnel received a text message"
        | Ok (Websocket.Close close) ->
            Error
              (match close.code with
              | None -> "SPDY tunnel closed"
              | Some code ->
                  Printf.sprintf "SPDY tunnel closed with code %d" code)
  in
  fill 0

let decode_header_block connection compressed =
  try
    let value =
      Spdy_zlib.inflate connection.inflater ~max_output:(1024 * 1024) compressed
    in
    parse_headers value
  with
  | Failure message -> Error message
  | exn -> Error (Printexc.to_string exn)

let read_wire_frame connection =
  let* header = tunnel_read_exact connection 8 in
  let first = get_u32 header 0 in
  let flags = Char.code header.[4] in
  let length = get_u24 header 5 in
  if length > 16 * 1024 * 1024 then Error "SPDY frame exceeds 16 MiB"
  else
    let* payload = tunnel_read_exact connection length in
    if first land 0x80000000 = 0 then
      let stream_id = first land 0x7fffffff in
      if stream_id = 0 then Error "SPDY data frame has stream ID zero"
      else Ok (Data { stream_id; fin = flags land 1 <> 0; payload })
    else
      let version = (first lsr 16) land 0x7fff in
      let frame_type = first land 0xffff in
      if version <> 3 then Error "SPDY control frame has an unsupported version"
      else
        match frame_type with
        | 1 ->
            if length < 10 then Error "truncated SPDY SYN_STREAM"
            else
              let* _headers =
                decode_header_block connection
                  (String.sub payload 10 (length - 10))
              in
              Ok Ignored
        | 2 ->
            if length < 4 then Error "truncated SPDY SYN_REPLY"
            else
              let stream_id = get_u32 payload 0 land 0x7fffffff in
              let* _headers =
                decode_header_block connection
                  (String.sub payload 4 (length - 4))
              in
              Ok (Syn_reply { stream_id; fin = flags land 1 <> 0 })
        | 3 ->
            if length <> 8 then Error "invalid SPDY RST_STREAM length"
            else
              Ok
                (Reset
                   {
                     stream_id = get_u32 payload 0 land 0x7fffffff;
                     status = get_u32 payload 4;
                   })
        | 4 ->
            if length < 4 then Error "truncated SPDY SETTINGS" else Ok Ignored
        | 6 ->
            if length <> 4 then Error "invalid SPDY PING length"
            else Ok (Ping (get_u32 payload 0))
        | 7 ->
            if length <> 8 then Error "invalid SPDY GOAWAY length"
            else
              Ok
                (Go_away
                   {
                     last_stream_id = get_u32 payload 0 land 0x7fffffff;
                     status = get_u32 payload 4;
                   })
        | 8 ->
            if length < 4 then Error "truncated SPDY HEADERS"
            else
              let* _headers =
                decode_header_block connection
                  (String.sub payload 4 (length - 4))
              in
              Ok Ignored
        | 9 ->
            if length <> 8 then Error "invalid SPDY WINDOW_UPDATE length"
            else Ok Ignored
        | _ ->
            Error
              (Printf.sprintf "unknown SPDY control frame type %d" frame_type)

let find_stream connection identifier =
  Mutex.lock connection.streams_lock;
  let stream = Hashtbl.find_opt connection.streams identifier in
  Mutex.unlock connection.streams_lock;
  stream

let unregister_stream stream =
  Mutex.lock stream.connection.streams_lock;
  Hashtbl.remove stream.connection.streams stream.stream_id;
  Mutex.unlock stream.connection.streams_lock

let fail_stream stream message =
  Mutex.lock stream.lock;
  if stream.failure = None then stream.failure <- Some message;
  Condition.broadcast stream.changed;
  Mutex.unlock stream.lock

let reset_wire stream =
  if Atomic.compare_and_set stream.reset_sent false true then
    ignore (send_reset stream.connection ~stream_id:stream.stream_id ~status:5)

let fail_connection connection message =
  Mutex.lock connection.close_lock;
  let first = not (Atomic.get connection.closed) in
  if first then (
    connection.close_reason <- message;
    Atomic.set connection.closed true);
  Mutex.unlock connection.close_lock;
  if first then (
    Cancel.cancel connection.ping_cancel;
    Websocket.close connection.socket;
    Mutex.lock connection.streams_lock;
    let streams = Hashtbl.to_seq_values connection.streams |> List.of_seq in
    Mutex.unlock connection.streams_lock;
    List.iter (fun stream -> fail_stream stream message) streams)

let dispatch connection = function
  | Syn_reply { stream_id; fin } -> (
      match find_stream connection stream_id with
      | None -> Ok ()
      | Some stream ->
          Mutex.lock stream.lock;
          if stream.replied then
            stream.failure <- Some "duplicate SPDY SYN_REPLY"
          else (
            stream.replied <- true;
            if fin then stream.remote_fin <- true);
          Condition.broadcast stream.changed;
          let retire = stream.local_fin && stream.remote_fin in
          Mutex.unlock stream.lock;
          if retire then unregister_stream stream;
          Ok ())
  | Data { stream_id; fin; payload } -> (
      match find_stream connection stream_id with
      | None -> Ok ()
      | Some stream ->
          Mutex.lock stream.lock;
          let after_fin = stream.remote_fin in
          let overflow =
            (not after_fin)
            && String.length payload
               > connection.max_stream_buffer_bytes - stream.queued_bytes
          in
          if after_fin then
            stream.failure <- Some "SPDY stream received data after FIN"
          else if overflow then
            stream.failure <-
              Some "SPDY stream receive buffer exceeded its limit"
          else (
            if payload <> "" then (
              Queue.add payload stream.queue;
              stream.queued_bytes <- stream.queued_bytes + String.length payload);
            if fin then stream.remote_fin <- true);
          Condition.broadcast stream.changed;
          let retire = stream.local_fin && stream.remote_fin in
          Mutex.unlock stream.lock;
          if retire then unregister_stream stream;
          if after_fin || overflow then (
            reset_wire stream;
            unregister_stream stream;
            Ok ())
          else Ok ())
  | Reset { stream_id; status } -> (
      match find_stream connection stream_id with
      | None -> Ok ()
      | Some stream ->
          fail_stream stream
            (Printf.sprintf "SPDY stream %d reset with status %d" stream_id
               status);
          unregister_stream stream;
          Ok ())
  | Ping identifier ->
      if identifier land 1 = 0 then send_ping connection identifier else Ok ()
  | Go_away { last_stream_id; status } ->
      Error
        (Printf.sprintf "SPDY GOAWAY after stream %d with status %d"
           last_stream_id status)
  | Ignored -> Ok ()

let reader_loop connection =
  let rec loop () =
    if not (Atomic.get connection.closed) then
      match read_wire_frame connection with
      | Error message -> fail_connection connection message
      | Ok frame -> (
          match dispatch connection frame with
          | Ok () -> loop ()
          | Error message -> fail_connection connection message)
  in
  loop ()

let ping_loop connection =
  let rec loop identifier =
    if Cancel.sleep connection.ping_cancel 10. then
      match send_ping connection identifier with
      | Ok () -> loop (if identifier >= 0x7ffffffd then 1 else identifier + 2)
      | Error message -> fail_connection connection message
  in
  loop 1

let create ?(max_stream_buffer_bytes = 4 * 1024 * 1024) socket =
  if max_stream_buffer_bytes <= 0 then
    Error "max_stream_buffer_bytes must be positive"
  else
    try
      let connection =
        {
          socket;
          deflater = Spdy_zlib.create_deflater dictionary;
          inflater = Spdy_zlib.create_inflater dictionary;
          write_lock = Mutex.create ();
          close_lock = Mutex.create ();
          streams_lock = Mutex.create ();
          streams = Hashtbl.create 17;
          next_stream_id = 1;
          last_stream_id = 0;
          max_stream_buffer_bytes;
          closed = Atomic.make false;
          close_reason = "SPDY connection is closed";
          tunnel_pending = "";
          tunnel_offset = 0;
          ping_cancel = Cancel.create ();
          reader_thread = None;
          ping_thread = None;
        }
      in
      let reader = Thread.create reader_loop connection in
      connection.reader_thread <- Some reader;
      let ping = Thread.create ping_loop connection in
      connection.ping_thread <- Some ping;
      Ok connection
    with
    | Failure message -> Error message
    | exn -> Error (Printexc.to_string exn)

let create_stream ?(timeout = 30.) connection headers =
  if (not (Float.is_finite timeout)) || timeout <= 0. then
    Error "SPDY stream creation timeout must be finite and positive"
  else if Atomic.get connection.closed then Error connection.close_reason
  else
    let stream =
      Mutex.lock connection.streams_lock;
      let identifier = connection.next_stream_id in
      if identifier > 0x7fffffff then (
        Mutex.unlock connection.streams_lock;
        None)
      else
        let stream =
          {
            connection;
            stream_id = identifier;
            send_lock = Mutex.create ();
            lock = Mutex.create ();
            changed = Condition.create ();
            queue = Queue.create ();
            queued_bytes = 0;
            current = None;
            current_offset = 0;
            replied = false;
            local_fin = false;
            remote_fin = false;
            failure = None;
            reset_sent = Atomic.make false;
          }
        in
        connection.next_stream_id <- identifier + 2;
        connection.last_stream_id <- identifier;
        Hashtbl.add connection.streams identifier stream;
        Mutex.unlock connection.streams_lock;
        Some stream
    in
    match stream with
    | None -> Error "SPDY stream identifiers are exhausted"
    | Some stream -> (
        match
          send_syn_stream connection ~stream_id:stream.stream_id headers
        with
        | Error message ->
            fail_stream stream message;
            Error message
        | Ok () ->
            let timer_cancel = Cancel.create () in
            let timer =
              Thread.create
                (fun () ->
                  if Cancel.sleep timer_cancel timeout then (
                    Mutex.lock stream.lock;
                    if (not stream.replied) && stream.failure = None then
                      stream.failure <- Some "SPDY stream creation timed out";
                    Condition.broadcast stream.changed;
                    Mutex.unlock stream.lock))
                ()
            in
            Mutex.lock stream.lock;
            while (not stream.replied) && stream.failure = None do
              Condition.wait stream.changed stream.lock
            done;
            let result =
              match stream.failure with
              | Some message -> Error message
              | None -> Ok stream
            in
            Mutex.unlock stream.lock;
            Cancel.cancel timer_cancel;
            Thread.join timer;
            (match result with
            | Ok _ -> ()
            | Error _ ->
                reset_wire stream;
                unregister_stream stream);
            result)

let read stream buffer offset length =
  if offset < 0 || length < 0 || offset > Bytes.length buffer - length then
    invalid_arg "Spdy.read: invalid byte range";
  if length = 0 then Ok 0
  else (
    Mutex.lock stream.lock;
    let rec wait () =
      let available =
        match stream.current with
        | Some value -> String.length value - stream.current_offset
        | None -> 0
      in
      if available > 0 then (
        let value = Option.get stream.current in
        let count = min available length in
        Bytes.blit_string value stream.current_offset buffer offset count;
        stream.current_offset <- stream.current_offset + count;
        stream.queued_bytes <- stream.queued_bytes - count;
        if stream.current_offset = String.length value then (
          stream.current <- None;
          stream.current_offset <- 0);
        Ok count)
      else if not (Queue.is_empty stream.queue) then (
        stream.current <- Some (Queue.take stream.queue);
        stream.current_offset <- 0;
        wait ())
      else
        match stream.failure with
        | Some message -> Error message
        | None when stream.remote_fin -> Ok 0
        | None ->
            Condition.wait stream.changed stream.lock;
            wait ()
    in
    let result = wait () in
    Mutex.unlock stream.lock;
    result)

let write stream value =
  Mutex.lock stream.send_lock;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock stream.send_lock)
    (fun () ->
      Mutex.lock stream.lock;
      let state =
        match stream.failure with
        | Some message -> Error message
        | None when stream.local_fin -> Error "SPDY stream write side is closed"
        | None -> Ok ()
      in
      Mutex.unlock stream.lock;
      let* () = state in
      let rec loop offset =
        if offset = String.length value then Ok ()
        else
          let count = min 16384 (String.length value - offset) in
          let payload = String.sub value offset count in
          let* () =
            send_data stream.connection ~stream_id:stream.stream_id ~fin:false
              payload
          in
          loop (offset + count)
      in
      loop 0)

let close_write stream =
  Mutex.lock stream.send_lock;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock stream.send_lock)
    (fun () ->
      Mutex.lock stream.lock;
      let should_send = (not stream.local_fin) && stream.failure = None in
      if should_send then stream.local_fin <- true;
      let failure = stream.failure in
      let retire = stream.local_fin && stream.remote_fin in
      Mutex.unlock stream.lock;
      let result =
        match failure with
        | Some message -> Error message
        | None when should_send ->
            send_data stream.connection ~stream_id:stream.stream_id ~fin:true ""
        | None -> Ok ()
      in
      if retire then unregister_stream stream;
      result)

let reset stream =
  fail_stream stream "SPDY stream reset locally";
  reset_wire stream;
  unregister_stream stream

let join_thread = function
  | None -> ()
  | Some thread ->
      if Thread.id thread <> Thread.id (Thread.self ()) then Thread.join thread

let close connection =
  if not (Atomic.get connection.closed) then ignore (send_go_away connection);
  fail_connection connection "SPDY connection closed locally";
  join_thread connection.reader_thread;
  join_thread connection.ping_thread

module For_testing = struct
  let dictionary_length = String.length dictionary
  let dictionary_adler32 = adler32 dictionary

  type peer = { deflater : Spdy_zlib.deflater; inflater : Spdy_zlib.inflater }

  let peer () =
    {
      deflater = Spdy_zlib.create_deflater dictionary;
      inflater = Spdy_zlib.create_inflater dictionary;
    }

  let decode_syn_stream peer frame =
    if String.length frame < 18 then Error "truncated SPDY SYN_STREAM"
    else
      let first = get_u32 frame 0 in
      let frame_type = first land 0xffff in
      let version = (first lsr 16) land 0x7fff in
      let length = get_u24 frame 5 in
      if first land 0x80000000 = 0 || version <> 3 || frame_type <> 1 then
        Error "frame is not a SPDY/3 SYN_STREAM"
      else if length <> String.length frame - 8 then
        Error "SPDY SYN_STREAM length mismatch"
      else
        let stream_id = get_u32 frame 8 land 0x7fffffff in
        try
          let headers =
            Spdy_zlib.inflate peer.inflater ~max_output:(1024 * 1024)
              (String.sub frame 18 (String.length frame - 18))
          in
          let* fields =
            parse_headers headers
            |> Result.map_error (fun message ->
                Printf.sprintf
                  "%s (compressed length %d, decoded header block length %d)"
                  message
                  (String.length frame - 18)
                  (String.length headers))
          in
          Ok (stream_id, fields)
        with Failure message -> Error message

  let syn_reply peer ~stream_id =
    let compressed =
      Spdy_zlib.deflate peer.deflater (encode_headers [ ("status", "200") ])
    in
    let payload = Buffer.create (4 + String.length compressed) in
    add_u32 payload stream_id;
    Buffer.add_string payload compressed;
    control_frame ~frame_type:2 ~flags:0 (Buffer.contents payload)

  let data ~stream_id ~fin payload = data_frame ~stream_id ~fin payload

  let decode_data frame =
    if String.length frame < 8 then Error "truncated SPDY data frame"
    else
      let first = get_u32 frame 0 in
      let length = get_u24 frame 5 in
      if first land 0x80000000 <> 0 then Error "frame is not SPDY data"
      else if length <> String.length frame - 8 then
        Error "SPDY data length mismatch"
      else
        Ok
          ( first land 0x7fffffff,
            Char.code frame.[4] land 1 <> 0,
            String.sub frame 8 length )
end
