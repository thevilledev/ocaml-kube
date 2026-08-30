module K = Kube

let fail message =
  Printf.eprintf "%s\n%!" message;
  exit 1

let remote_error context error =
  fail (context ^ ": " ^ Format.asprintf "%a" K.Remote_command.pp_error error)

let port_error context error =
  fail (context ^ ": " ^ Format.asprintf "%a" K.Port_forward.pp_error error)

let contains value fragment =
  let value_length = String.length value in
  let fragment_length = String.length fragment in
  let rec loop offset =
    if offset > value_length - fragment_length then false
    else if String.sub value offset fragment_length = fragment then true
    else loop (offset + 1)
  in
  fragment = "" || loop 0

let with_deadline seconds fn =
  let parent = K.Cancel.create () in
  let result, timed_out = K.Cancel.with_timeout ~parent seconds fn in
  if timed_out then fail "streaming operation timed out" else result

let check_exec client ~namespace ~pod =
  with_deadline 30. (fun cancel ->
      match
        K.Remote_command.exec ~cancel ~namespace ~container:"server" client ~pod
          ~command:[ "sh"; "-c"; "printf exec-ok" ]
          ()
      with
      | Error error -> remote_error "exec upgrade" error
      | Ok session ->
          Fun.protect
            ~finally:(fun () -> K.Remote_command.close session)
            (fun () ->
              let output = Buffer.create 64 in
              let rec receive () =
                match K.Remote_command.receive session with
                | Error error -> remote_error "exec stream" error
                | Ok (K.Remote_command.Stdout_data value)
                | Ok (K.Remote_command.Stderr_data value) ->
                    Buffer.add_string output value;
                    receive ()
                | Ok (K.Remote_command.Exit K.Remote_command.Success) -> ()
                | Ok (K.Remote_command.Exit (K.Remote_command.Exit_code code))
                  -> fail (Printf.sprintf "exec returned exit code %d" code)
                | Ok (K.Remote_command.Remote_error status) ->
                    fail ("exec remote error: " ^ status.message)
                | Ok (K.Remote_command.Connection_closed _) ->
                    fail "exec connection closed before its exit status"
                | Ok (K.Remote_command.Stream_closed _) -> receive ()
              in
              receive ();
              if not (contains (Buffer.contents output) "exec-ok") then
                fail "exec stdout did not contain exec-ok"))

let check_attach client ~namespace ~pod =
  with_deadline 30. (fun cancel ->
      match
        K.Remote_command.attach ~cancel ~namespace ~container:"attached"
          ~stdin:true client ~pod ()
      with
      | Error error -> remote_error "attach upgrade" error
      | Ok session ->
          Fun.protect
            ~finally:(fun () -> K.Remote_command.close session)
            (fun () ->
              (match K.Remote_command.send_stdin session "attach-ok\n" with
              | Ok () -> ()
              | Error error -> remote_error "attach stdin" error);
              (match K.Remote_command.close_stdin session with
              | Ok () -> ()
              | Error error -> remote_error "attach close stdin" error);
              let output = Buffer.create 64 in
              let rec receive () =
                if contains (Buffer.contents output) "attach-ok" then ()
                else
                  match K.Remote_command.receive session with
                  | Error error -> remote_error "attach stream" error
                  | Ok (K.Remote_command.Stdout_data value)
                  | Ok (K.Remote_command.Stderr_data value) ->
                      Buffer.add_string output value;
                      receive ()
                  | Ok (K.Remote_command.Remote_error status) ->
                      fail ("attach remote error: " ^ status.message)
                  | Ok (K.Remote_command.Connection_closed _)
                  | Ok (K.Remote_command.Exit _) ->
                      if not (contains (Buffer.contents output) "attach-ok")
                      then fail "attach ended before echoing stdin"
                  | Ok (K.Remote_command.Stream_closed _) -> receive ()
              in
              receive ()))

let write_all descriptor value =
  let rec loop offset =
    if offset < String.length value then
      let count =
        Unix.write_substring descriptor value offset
          (String.length value - offset)
      in
      if count = 0 then fail "local port-forward write returned zero"
      else loop (offset + count)
  in
  loop 0

let check_port_forward client ~namespace ~pod =
  with_deadline 30. (fun cancel ->
      let mapping =
        K.Port_forward.Forwarder.{ local_port = 0; remote_port = 8080 }
      in
      match
        K.Port_forward.Forwarder.start ~cancel ~namespace client ~pod
          ~ports:[ mapping ] ()
      with
      | Error error -> port_error "port-forward upgrade" error
      | Ok forwarder ->
          Fun.protect
            ~finally:(fun () -> K.Port_forward.Forwarder.close forwarder)
            (fun () ->
              let port =
                match K.Port_forward.Forwarder.bound_ports forwarder with
                | [ value ] -> value.local_port
                | _ -> fail "port-forward bound an unexpected listener count"
              in
              let descriptor = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
              Fun.protect
                ~finally:(fun () ->
                  try Unix.close descriptor with Unix.Unix_error _ -> ())
                (fun () ->
                  Unix.connect descriptor
                    (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
                  write_all descriptor
                    "GET /proof.txt HTTP/1.1\r\n\
                     Host: pod\r\n\
                     Connection: close\r\n\
                     \r\n";
                  let buffer = Bytes.create 4096 in
                  let response = Buffer.create 4096 in
                  let rec read () =
                    match
                      Unix.read descriptor buffer 0 (Bytes.length buffer)
                    with
                    | 0 -> ()
                    | count ->
                        Buffer.add_subbytes response buffer 0 count;
                        read ()
                  in
                  read ();
                  let response = Buffer.contents response in
                  if not (contains response "200 OK") then
                    fail "port-forward HTTP response was not successful";
                  if not (contains response "port-forward-ok") then
                    fail "port-forward response did not contain the Pod payload")))

let () =
  let kubeconfig = ref None in
  let context = ref None in
  let namespace = ref "default" in
  let pod = ref "ocaml-k8s-streaming-check" in
  let set option value = option := Some value in
  Arg.parse
    [
      ("--kubeconfig", Arg.String (set kubeconfig), "PATH Kubeconfig path");
      ("--context", Arg.String (set context), "NAME Kubeconfig context");
      ("--namespace", Arg.Set_string namespace, "NAME Pod namespace");
      ("--pod", Arg.Set_string pod, "NAME Pod name");
    ]
    (fun value -> raise (Arg.Bad ("unexpected argument: " ^ value)))
    "streaming-check [OPTIONS]";
  let config =
    match !kubeconfig with
    | Some path -> K.Config.load_kubeconfig ?context:!context path
    | None -> K.Config.load_default ?context:!context ()
  in
  let config =
    match config with
    | Ok config -> config
    | Error message -> fail ("configuration error: " ^ message)
  in
  let client = K.Client.create config in
  Fun.protect
    ~finally:(fun () -> K.Client.close client)
    (fun () ->
      check_exec client ~namespace:!namespace ~pod:!pod;
      check_attach client ~namespace:!namespace ~pod:!pod;
      check_port_forward client ~namespace:!namespace ~pod:!pod;
      Printf.printf "streaming passed: exec, attach, and port-forward\n%!")
