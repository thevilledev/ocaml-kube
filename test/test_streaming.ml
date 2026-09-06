module K = Kube

let write_all descriptor value =
  let rec loop offset =
    if offset < String.length value then
      let count =
        Unix.write_substring descriptor value offset
          (String.length value - offset)
      in
      if count = 0 then raise End_of_file else loop (offset + count)
  in
  loop 0

let read_exact descriptor length =
  let output = Bytes.create length in
  let rec loop offset =
    if offset < length then
      let count = Unix.read descriptor output offset (length - offset) in
      if count = 0 then raise End_of_file else loop (offset + count)
  in
  loop 0;
  Bytes.unsafe_to_string output

let read_head descriptor =
  let output = Buffer.create 1024 in
  let byte = Bytes.create 1 in
  let rec loop state =
    if state = 4 then Buffer.contents output
    else
      let count = Unix.read descriptor byte 0 1 in
      if count = 0 then raise End_of_file;
      let character = Bytes.get byte 0 in
      Buffer.add_char output character;
      let state =
        match (state, character) with
        | 0, '\r' -> 1
        | 1, '\n' -> 2
        | 2, '\r' -> 3
        | 3, '\n' -> 4
        | _, '\r' -> 1
        | _ -> 0
      in
      loop state
  in
  loop 0

let request_header name request =
  let name = String.lowercase_ascii name in
  String.split_on_char '\n' request
  |> List.find_map (fun line ->
      match String.index_opt line ':' with
      | None -> None
      | Some separator ->
          let candidate =
            String.sub line 0 separator |> String.trim |> String.lowercase_ascii
          in
          if candidate <> name then None
          else
            Some
              (String.sub line (separator + 1)
                 (String.length line - separator - 1)
              |> String.trim))

let server_frame ?(final = true) opcode payload =
  let length = String.length payload in
  if length > 65535 then invalid_arg "test server frame is too large";
  let extended = if length <= 125 then 0 else 2 in
  let output = Bytes.create (2 + extended + length) in
  Bytes.set output 0 (Char.chr ((if final then 0x80 else 0) lor opcode));
  Bytes.set output 1 (Char.chr (if extended = 0 then length else 126));
  if extended = 2 then (
    Bytes.set output 2 (Char.chr ((length lsr 8) land 0xff));
    Bytes.set output 3 (Char.chr (length land 0xff)));
  Bytes.blit_string payload 0 output (2 + extended) length;
  Bytes.unsafe_to_string output

let channel identifier payload = String.make 1 (Char.chr identifier) ^ payload

let accept_websocket descriptor request protocol =
  let key = request_header "sec-websocket-key" request |> Option.get in
  let response =
    "HTTP/1.1 101 Switching Protocols\r\n" ^ "Upgrade: websocket\r\n"
    ^ "Connection: Upgrade\r\n" ^ "Sec-WebSocket-Protocol: " ^ protocol ^ "\r\n"
    ^ "Sec-WebSocket-Accept: "
    ^ K.Websocket.For_testing.handshake_accept key
    ^ "\r\n\r\n"
  in
  write_all descriptor response

type client_frame = { opcode : int; payload : string }

let read_client_frame descriptor =
  let head = read_exact descriptor 2 in
  let first = Char.code head.[0] in
  let second = Char.code head.[1] in
  if second land 0x80 = 0 then failwith "client frame was not masked";
  let marker = second land 0x7f in
  let length =
    if marker <= 125 then marker
    else if marker = 126 then
      let value = read_exact descriptor 2 in
      (Char.code value.[0] lsl 8) lor Char.code value.[1]
    else failwith "test frame is unexpectedly large"
  in
  let mask = read_exact descriptor 4 in
  let payload = Bytes.of_string (read_exact descriptor length) in
  for index = 0 to length - 1 do
    Bytes.set payload index
      (Char.chr
         (Char.code (Bytes.get payload index) lxor Char.code mask.[index land 3]))
  done;
  { opcode = first land 0x0f; payload = Bytes.unsafe_to_string payload }

let with_server serve fn =
  let listener = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt listener Unix.SO_REUSEADDR true;
  Unix.bind listener (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen listener 1;
  let port =
    match Unix.getsockname listener with
    | Unix.ADDR_INET (_, port) -> port
    | Unix.ADDR_UNIX _ -> assert false
  in
  let server_result = Atomic.make None in
  let worker =
    Thread.create
      (fun () ->
        let result =
          try
            let descriptor, _ = Unix.accept listener in
            Fun.protect
              ~finally:(fun () -> Unix.close descriptor)
              (fun () -> Ok (serve descriptor))
          with exn -> Error (Printexc.to_string exn)
        in
        Atomic.set server_result (Some result))
      ()
  in
  let config =
    K.Config.make ~credential:(K.Config.Static_token "stream-token")
      (Uri.of_string (Printf.sprintf "http://127.0.0.1:%d" port))
  in
  let client_result =
    Fun.protect
      ~finally:(fun () -> Unix.close listener)
      (fun () -> try Ok (fn config) with exn -> Error exn)
  in
  Thread.join worker;
  match Atomic.get server_result with
  | Some (Ok server_value) -> (
      match client_result with
      | Ok client_value -> (client_value, server_value)
      | Error exn -> raise exn)
  | Some (Error message) -> (
      match client_result with
      | Ok _ -> Alcotest.fail ("server failed: " ^ message)
      | Error exn ->
          Alcotest.failf "server failed: %s; client failed: %s" message
            (Printexc.to_string exn))
  | None -> Alcotest.fail "server produced no result"

let test_handshake_accept () =
  Alcotest.(check string)
    "RFC example" "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="
    (K.Websocket.For_testing.handshake_accept "dGhlIHNhbXBsZSBub25jZQ==")

let test_websocket_framing () =
  let serve descriptor =
    let request = read_head descriptor in
    accept_websocket descriptor request "v5.channel.k8s.io";
    write_all descriptor (server_frame 9 "heartbeat");
    write_all descriptor (server_frame ~final:false 2 "hel");
    write_all descriptor (server_frame 0 "lo");
    let pong = read_client_frame descriptor in
    let data = read_client_frame descriptor in
    let close = read_client_frame descriptor in
    (request, pong, data, close)
  in
  let run config =
    let client = K.Client.create config in
    Fun.protect
      ~finally:(fun () -> K.Client.close client)
      (fun () ->
        match
          K.Client.websocket client "/stream?value=1"
            ~protocols:[ "v5.channel.k8s.io" ]
        with
        | Error error ->
            Alcotest.failf "upgrade failed: %a" K.Client.pp_error error
        | Ok socket ->
            Alcotest.(check (option string))
              "selected protocol" (Some "v5.channel.k8s.io")
              (K.Websocket.protocol socket);
            (match K.Websocket.receive socket with
            | Ok (K.Websocket.Binary value) ->
                Alcotest.(check string) "fragmented binary" "hello" value
            | Ok _ -> Alcotest.fail "unexpected WebSocket message"
            | Error message -> Alcotest.fail message);
            (match K.Websocket.send_binary socket "world" with
            | Ok () -> ()
            | Error message -> Alcotest.fail message);
            K.Websocket.close socket)
  in
  let (), (request, pong, data, close) = with_server serve run in
  Alcotest.(check bool)
    "target" true
    (String.starts_with ~prefix:"GET /stream?value=1 " request);
  Alcotest.(check (option string))
    "authorization" (Some "Bearer stream-token")
    (request_header "authorization" request);
  Alcotest.(check int) "pong opcode" 10 pong.opcode;
  Alcotest.(check string) "pong payload" "heartbeat" pong.payload;
  Alcotest.(check int) "binary opcode" 2 data.opcode;
  Alcotest.(check string) "binary payload" "world" data.payload;
  Alcotest.(check int) "close opcode" 8 close.opcode

let test_upgrade_status_error () =
  let body =
    Yojson.Safe.to_string
      (`Assoc
         [
           ("kind", `String "Status");
           ("status", `String "Failure");
           ("reason", `String "Forbidden");
           ("message", `String "stream denied");
           ("code", `Int 403);
         ])
  in
  Fake_api_server.with_server
    [ Fake_api_server.fixed ~status:"403 Forbidden" body ]
    (fun server ->
      let client = K.Client.create (Fake_api_server.config server) in
      Fun.protect
        ~finally:(fun () -> K.Client.close client)
        (fun () ->
          match K.Client.websocket client "/stream" with
          | Error error ->
              Alcotest.(check bool)
                "classified" true
                (K.Client.Error.is_forbidden error)
          | Ok socket ->
              K.Websocket.close socket;
              Alcotest.fail "forbidden upgrade unexpectedly succeeded"))

let request_target request =
  match String.split_on_char ' ' request with
  | _meth :: target :: _ -> target
  | _ -> Alcotest.fail "invalid request line"

let query_values name target =
  Uri.of_string target |> Uri.query
  |> List.filter_map (fun (candidate, values) ->
      if candidate = name then Some values else None)
  |> List.concat

let test_exec_session () =
  let serve descriptor =
    let request = read_head descriptor in
    accept_websocket descriptor request "v5.channel.k8s.io";
    let stdin = read_client_frame descriptor in
    let resize = read_client_frame descriptor in
    let close_stdin = read_client_frame descriptor in
    write_all descriptor (server_frame 2 (channel 1 "command output"));
    write_all descriptor (server_frame 2 (channel 255 "\001"));
    let status =
      `Assoc
        [
          ("status", `String "Failure");
          ("reason", `String "NonZeroExitCode");
          ("message", `String "command terminated with exit code 7");
          ("code", `Int 500);
          ( "details",
            `Assoc
              [
                ( "causes",
                  `List
                    [
                      `Assoc
                        [
                          ("type", `String "ExitCode"); ("message", `String "7");
                        ];
                    ] );
              ] );
        ]
      |> Yojson.Safe.to_string
    in
    write_all descriptor (server_frame 2 (channel 3 status));
    let close = read_client_frame descriptor in
    (request, stdin, resize, close_stdin, close)
  in
  let run config =
    let client = K.Client.create config in
    Fun.protect
      ~finally:(fun () -> K.Client.close client)
      (fun () ->
        match
          K.Remote_command.exec ~namespace:"operators" ~container:"worker"
            ~stdin:true ~tty:true ~stderr:true client ~pod:"demo"
            ~command:[ "sh"; "-c"; "exit 7" ] ()
        with
        | Error error ->
            Alcotest.failf "exec failed: %a" K.Remote_command.pp_error error
        | Ok session ->
            Alcotest.(check bool)
              "v5" true
              (K.Remote_command.protocol session = K.Remote_command.V5);
            (match K.Remote_command.send_stdin session "input" with
            | Ok () -> ()
            | Error error -> Alcotest.failf "%a" K.Remote_command.pp_error error);
            (match K.Remote_command.resize session ~width:80 ~height:24 with
            | Ok () -> ()
            | Error error -> Alcotest.failf "%a" K.Remote_command.pp_error error);
            (match K.Remote_command.close_stdin session with
            | Ok () -> ()
            | Error error -> Alcotest.failf "%a" K.Remote_command.pp_error error);
            (match K.Remote_command.receive session with
            | Ok (K.Remote_command.Stdout_data value) ->
                Alcotest.(check string) "stdout" "command output" value
            | Ok _ -> Alcotest.fail "unexpected first exec event"
            | Error error -> Alcotest.failf "%a" K.Remote_command.pp_error error);
            (match K.Remote_command.receive session with
            | Ok (K.Remote_command.Stream_closed K.Remote_command.Stdout) -> ()
            | Ok _ -> Alcotest.fail "unexpected stream-close event"
            | Error error -> Alcotest.failf "%a" K.Remote_command.pp_error error);
            (match K.Remote_command.receive session with
            | Ok (K.Remote_command.Exit (K.Remote_command.Exit_code 7)) -> ()
            | Ok _ -> Alcotest.fail "unexpected exit event"
            | Error error -> Alcotest.failf "%a" K.Remote_command.pp_error error);
            K.Remote_command.close session)
  in
  let (), (request, stdin, resize, close_stdin, close) =
    with_server serve run
  in
  let target = request_target request in
  Alcotest.(check string)
    "exec path" "/api/v1/namespaces/operators/pods/demo/exec"
    (Uri.of_string target |> Uri.path);
  Alcotest.(check (list string))
    "command query" [ "sh"; "-c"; "exit 7" ]
    (query_values "command" target);
  Alcotest.(check (list string))
    "TTY disables stderr" [ "false" ]
    (query_values "stderr" target);
  Alcotest.(check string) "stdin channel" (channel 0 "input") stdin.payload;
  Alcotest.(check int) "stdin opcode" 2 stdin.opcode;
  let resize_json =
    String.sub resize.payload 1 (String.length resize.payload - 1)
    |> Yojson.Safe.from_string
  in
  Alcotest.(check int) "resize channel" 4 (Char.code resize.payload.[0]);
  Alcotest.(check int)
    "resize width" 80
    Yojson.Safe.Util.(resize_json |> member "Width" |> to_int);
  Alcotest.(check string) "stdin close" (channel 255 "\000") close_stdin.payload;
  Alcotest.(check int) "session close" 8 close.opcode

let test_attach_path () =
  let serve descriptor =
    let request = read_head descriptor in
    accept_websocket descriptor request "v5.channel.k8s.io";
    write_all descriptor (server_frame 8 "");
    ignore (read_client_frame descriptor);
    request
  in
  let run config =
    let client = K.Client.create config in
    Fun.protect
      ~finally:(fun () -> K.Client.close client)
      (fun () ->
        match K.Remote_command.attach client ~pod:"demo" () with
        | Error error ->
            Alcotest.failf "attach failed: %a" K.Remote_command.pp_error error
        | Ok session ->
            (match K.Remote_command.receive session with
            | Ok (K.Remote_command.Connection_closed _) -> ()
            | Ok _ -> Alcotest.fail "unexpected attach event"
            | Error error -> Alcotest.failf "%a" K.Remote_command.pp_error error);
            K.Remote_command.close session)
  in
  let (), request = with_server serve run in
  let target = request_target request in
  Alcotest.(check string)
    "attach path" "/api/v1/namespaces/default/pods/demo/attach"
    (Uri.of_string target |> Uri.path);
  Alcotest.(check (list string)) "no command" [] (query_values "command" target)

let test_port_forwarder () =
  let serve descriptor =
    let request = read_head descriptor in
    accept_websocket descriptor request "SPDY/3.1+portforward.k8s.io";
    let peer = K.Port_forward.For_testing.peer () in
    let receive_syn expected_type =
      let frame = read_client_frame descriptor in
      Alcotest.(check int) "SYN WebSocket opcode" 2 frame.opcode;
      match K.Port_forward.For_testing.decode_syn_stream peer frame.payload with
      | Error message -> Alcotest.fail message
      | Ok (stream_id, headers) ->
          Alcotest.(check string)
            "stream type" expected_type
            (List.assoc "streamtype" headers);
          Alcotest.(check string)
            "port header" "8080"
            (List.assoc "port" headers);
          Alcotest.(check string)
            "request ID" "0"
            (List.assoc "requestid" headers);
          write_all descriptor
            (server_frame 2
               (K.Port_forward.For_testing.syn_reply peer ~stream_id));
          stream_id
    in
    let error_stream = receive_syn "error" in
    let error_fin = read_client_frame descriptor in
    (match K.Port_forward.For_testing.decode_data error_fin.payload with
    | Ok (stream_id, true, "") ->
        Alcotest.(check int) "error stream FIN" error_stream stream_id
    | Ok _ -> Alcotest.fail "unexpected error-stream data"
    | Error message -> Alcotest.fail message);
    let data_stream = receive_syn "data" in
    let input = Buffer.create 16 in
    let rec receive_input () =
      let frame = read_client_frame descriptor in
      match K.Port_forward.For_testing.decode_data frame.payload with
      | Error message -> Alcotest.fail message
      | Ok (stream_id, fin, payload) ->
          Alcotest.(check int) "data stream ID" data_stream stream_id;
          Buffer.add_string input payload;
          if not fin then receive_input ()
    in
    receive_input ();
    write_all descriptor
      (server_frame 2
         (K.Port_forward.For_testing.data ~stream_id:error_stream ~fin:true ""));
    write_all descriptor
      (server_frame 2
         (K.Port_forward.For_testing.data ~stream_id:data_stream ~fin:true
            "pong"));
    let rec await_close () =
      let frame = read_client_frame descriptor in
      if frame.opcode <> 8 then await_close ()
    in
    await_close ();
    (request, Buffer.contents input)
  in
  let run config =
    let client = K.Client.create config in
    Fun.protect
      ~finally:(fun () -> K.Client.close client)
      (fun () ->
        let mapping =
          K.Port_forward.Forwarder.{ local_port = 0; remote_port = 8080 }
        in
        match
          K.Port_forward.Forwarder.start client ~pod:"demo" ~ports:[ mapping ]
            ()
        with
        | Error error ->
            Alcotest.failf "port-forward failed: %a" K.Port_forward.pp_error
              error
        | Ok forwarder ->
            Fun.protect
              ~finally:(fun () -> K.Port_forward.Forwarder.close forwarder)
              (fun () ->
                let bound = K.Port_forward.Forwarder.bound_ports forwarder in
                let port =
                  match bound with
                  | [ value ] -> value.local_port
                  | _ -> Alcotest.fail "unexpected bound-port count"
                in
                let descriptor = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
                Fun.protect
                  ~finally:(fun () ->
                    try Unix.close descriptor with Unix.Unix_error _ -> ())
                  (fun () ->
                    Unix.connect descriptor
                      (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
                    write_all descriptor "ping";
                    Unix.shutdown descriptor Unix.SHUTDOWN_SEND;
                    let response = read_exact descriptor 4 in
                    Alcotest.(check string) "forwarded response" "pong" response;
                    let probe = Bytes.create 1 in
                    Alcotest.(check int)
                      "local EOF" 0
                      (Unix.read descriptor probe 0 1))))
  in
  let (), (request, input) = with_server serve run in
  let target = request_target request in
  Alcotest.(check string)
    "port-forward path" "/api/v1/namespaces/default/pods/demo/portforward"
    (Uri.of_string target |> Uri.path);
  Alcotest.(check (list string))
    "port query" [ "8080" ]
    (query_values "port" target);
  Alcotest.(check string) "forwarded input" "ping" input

let test_port_forwarder_recovers_from_stream_error () =
  let serve descriptor =
    let request = read_head descriptor in
    accept_websocket descriptor request "SPDY/3.1+portforward.k8s.io";
    let peer = K.Port_forward.For_testing.peer () in
    let rec receive_syn expected_type =
      let frame = read_client_frame descriptor in
      match K.Port_forward.For_testing.decode_syn_stream peer frame.payload with
      | Error _ -> receive_syn expected_type
      | Ok (stream_id, headers) ->
          Alcotest.(check string)
            "stream type" expected_type
            (List.assoc "streamtype" headers);
          write_all descriptor
            (server_frame 2
               (K.Port_forward.For_testing.syn_reply peer ~stream_id));
          stream_id
    in
    let receive_error_fin stream_id =
      let rec loop () =
        let frame = read_client_frame descriptor in
        match K.Port_forward.For_testing.decode_data frame.payload with
        | Ok (candidate, true, "") when candidate = stream_id -> ()
        | Ok _ | Error _ -> loop ()
      in
      loop ()
    in
    let first_error = receive_syn "error" in
    receive_error_fin first_error;
    let first_data = receive_syn "data" in
    write_all descriptor
      (server_frame 2
         (K.Port_forward.For_testing.reset ~stream_id:first_data ~status:2));
    let second_error = receive_syn "error" in
    receive_error_fin second_error;
    let second_data = receive_syn "data" in
    let input = Buffer.create 16 in
    let rec receive_input () =
      let frame = read_client_frame descriptor in
      match K.Port_forward.For_testing.decode_data frame.payload with
      | Ok (stream_id, fin, payload) when stream_id = second_data ->
          Buffer.add_string input payload;
          if not fin then receive_input ()
      | Ok _ | Error _ -> receive_input ()
    in
    receive_input ();
    write_all descriptor
      (server_frame 2
         (K.Port_forward.For_testing.data ~stream_id:second_error ~fin:true ""));
    write_all descriptor
      (server_frame 2
         (K.Port_forward.For_testing.data ~stream_id:second_data ~fin:true
            "pong"));
    let rec await_close () =
      let frame = read_client_frame descriptor in
      if frame.opcode <> 8 then await_close ()
    in
    await_close ();
    (request, Buffer.contents input)
  in
  let run config =
    let client = K.Client.create config in
    Fun.protect
      ~finally:(fun () -> K.Client.close client)
      (fun () ->
        let errors = Atomic.make 0 in
        let mapping =
          K.Port_forward.Forwarder.{ local_port = 0; remote_port = 8080 }
        in
        match
          K.Port_forward.Forwarder.start
            ~on_connection_error:(fun _ -> Atomic.incr errors)
            client ~pod:"demo" ~ports:[ mapping ] ()
        with
        | Error error ->
            Alcotest.failf "port-forward failed: %a" K.Port_forward.pp_error
              error
        | Ok forwarder ->
            Fun.protect
              ~finally:(fun () -> K.Port_forward.Forwarder.close forwarder)
              (fun () ->
                let port =
                  match K.Port_forward.Forwarder.bound_ports forwarder with
                  | [ value ] -> value.local_port
                  | _ -> Alcotest.fail "unexpected bound-port count"
                in
                let connect () =
                  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
                  Unix.connect socket
                    (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
                  socket
                in
                let first = connect () in
                Fun.protect
                  ~finally:(fun () -> Unix.close first)
                  (fun () ->
                    write_all first "discard";
                    Unix.shutdown first Unix.SHUTDOWN_SEND;
                    let probe = Bytes.create 1 in
                    let closed =
                      try Unix.read first probe 0 1 = 0
                      with Unix.Unix_error (Unix.ECONNRESET, _, _) -> true
                    in
                    Alcotest.(check bool)
                      "failed connection closes locally" true closed);
                let deadline = Unix.gettimeofday () +. 2. in
                while
                  Atomic.get errors = 0 && Unix.gettimeofday () < deadline
                do
                  Thread.delay 0.001
                done;
                Alcotest.(check int)
                  "stream failure reported" 1 (Atomic.get errors);
                let second = connect () in
                Fun.protect
                  ~finally:(fun () -> Unix.close second)
                  (fun () ->
                    write_all second "ping";
                    Unix.shutdown second Unix.SHUTDOWN_SEND;
                    Alcotest.(check string)
                      "next connection succeeds" "pong" (read_exact second 4))))
  in
  let (), (request, input) = with_server serve run in
  Alcotest.(check (list string))
    "requested port" [ "8080" ]
    (request_target request |> query_values "port");
  Alcotest.(check string) "second input forwarded" "ping" input

let test_port_forward_remote_error () =
  let serve descriptor =
    let request = read_head descriptor in
    accept_websocket descriptor request "SPDY/3.1+portforward.k8s.io";
    let peer = K.Port_forward.For_testing.peer () in
    let receive_syn () =
      let frame = read_client_frame descriptor in
      match K.Port_forward.For_testing.decode_syn_stream peer frame.payload with
      | Error message -> Alcotest.fail message
      | Ok (stream_id, _) ->
          write_all descriptor
            (server_frame 2
               (K.Port_forward.For_testing.syn_reply peer ~stream_id));
          stream_id
    in
    let error_stream = receive_syn () in
    ignore (read_client_frame descriptor);
    let _data_stream = receive_syn () in
    write_all descriptor
      (server_frame 2
         (K.Port_forward.For_testing.data ~stream_id:error_stream ~fin:true
            "dial tcp: connection refused"));
    let rec await_close () =
      match read_client_frame descriptor with
      | { opcode = 8; _ } -> ()
      | _ -> await_close ()
    in
    await_close ();
    request
  in
  let run config =
    let client = K.Client.create config in
    Fun.protect
      ~finally:(fun () -> K.Client.close client)
      (fun () ->
        match K.Port_forward.connect ~ports:[ 8080 ] client ~pod:"demo" () with
        | Error error ->
            Alcotest.failf "port-forward failed: %a" K.Port_forward.pp_error
              error
        | Ok connection ->
            Fun.protect
              ~finally:(fun () -> K.Port_forward.close connection)
              (fun () ->
                match K.Port_forward.open_stream connection ~port:8080 with
                | Error error ->
                    Alcotest.failf "stream failed: %a" K.Port_forward.pp_error
                      error
                | Ok stream ->
                    Fun.protect
                      ~finally:(fun () -> K.Port_forward.close_stream stream)
                      (fun () ->
                        let buffer = Bytes.create 16 in
                        match K.Port_forward.read stream buffer 0 16 with
                        | Error (K.Port_forward.Protocol_error message) ->
                            Alcotest.(check string)
                              "remote error" "dial tcp: connection refused"
                              message
                        | Error error ->
                            Alcotest.failf "unexpected error: %a"
                              K.Port_forward.pp_error error
                        | Ok _ ->
                            Alcotest.fail
                              "remote stream error was not propagated")))
  in
  let (), request = with_server serve run in
  Alcotest.(check (list string))
    "requested port" [ "8080" ]
    (request_target request |> query_values "port")

let () =
  Alcotest.run "streaming transport"
    [
      ( "websocket",
        [
          Alcotest.test_case "handshake accept" `Quick test_handshake_accept;
          Alcotest.test_case "framing" `Quick test_websocket_framing;
          Alcotest.test_case "Status error" `Quick test_upgrade_status_error;
        ] );
      ( "remote command",
        [
          Alcotest.test_case "exec session" `Quick test_exec_session;
          Alcotest.test_case "attach path" `Quick test_attach_path;
        ] );
      ( "port forward",
        [
          Alcotest.test_case "local forwarder" `Quick test_port_forwarder;
          Alcotest.test_case "stream error recovery" `Quick
            test_port_forwarder_recovers_from_stream_error;
          Alcotest.test_case "remote error wakes data" `Quick
            test_port_forward_remote_error;
        ] );
    ]
