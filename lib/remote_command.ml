type protocol = V1 | V2 | V3 | V4 | V5
type stream = Stdin | Stdout | Stderr | Error_stream | Resize
type exit_status = Success | Exit_code of int

type remote_status = {
  status : string option;
  reason : string option;
  message : string;
  code : int option;
  body : Yojson.Safe.t option;
}

type event =
  | Stdout_data of string
  | Stderr_data of string
  | Stream_closed of stream
  | Exit of exit_status
  | Remote_error of remote_status
  | Connection_closed of Websocket.close

type error = Client_error of Client.error | Protocol_error of string

type t = {
  socket : Websocket.t;
  protocol : protocol;
  stdin : bool;
  stdout : bool;
  stderr : bool;
  tty : bool;
  stdin_closed : bool Atomic.t;
}

let ( let* ) result fn =
  match result with
  | Ok value -> fn value
  | Error _ as error -> error

let pp_error formatter = function
  | Client_error error -> Client.pp_error formatter error
  | Protocol_error message ->
      Format.fprintf formatter "remote-command protocol error: %s" message

let protocol session = session.protocol

let protocol_name = function
  | V1 -> "channel.k8s.io"
  | V2 -> "v2.channel.k8s.io"
  | V3 -> "v3.channel.k8s.io"
  | V4 -> "v4.channel.k8s.io"
  | V5 -> "v5.channel.k8s.io"

let protocol_of_name = function
  | Some "v5.channel.k8s.io" -> Ok V5
  | Some "v4.channel.k8s.io" -> Ok V4
  | Some "v3.channel.k8s.io" -> Ok V3
  | Some "v2.channel.k8s.io" -> Ok V2
  | Some "channel.k8s.io" | None -> Ok V1
  | Some value ->
      Error (Protocol_error ("unknown negotiated protocol " ^ value))

let protocol_number = function
  | V1 -> 1
  | V2 -> 2
  | V3 -> 3
  | V4 -> 4
  | V5 -> 5

let pod_api =
  {
    Core.group = "";
    version = "v1";
    kind = "Pod";
    plural = "pods";
    scope = Core.Namespaced;
  }

let bool value = if value then "true" else "false"

let namespace client = function
  | Some namespace -> namespace
  | None ->
      Option.value ~default:"default" (Client.config client).Config.namespace

let validate_name field value =
  if value = "" then Error (Protocol_error (field ^ " must not be empty"))
  else Ok ()

let connect ?cancel ?namespace:requested_namespace ?container ?(stdin = false)
    ?(stdout = true) ?(stderr = true) ?(tty = false) ?(protocols = [ V5 ])
    client ~pod ~subresource ~command () =
  let* () = validate_name "pod" pod in
  let namespace = namespace client requested_namespace in
  let* () = validate_name "namespace" namespace in
  let* () =
    match container with
    | Some value -> validate_name "container" value
    | None -> Ok ()
  in
  let* () =
    if protocols = [] then
      Error (Protocol_error "at least one stream protocol is required")
    else Ok ()
  in
  let stderr = stderr && not tty in
  let* () =
    if stdin || stdout || stderr then Ok ()
    else Error (Protocol_error "at least one stream must be enabled")
  in
  let* path =
    match
      Core.subresource_path pod_api ~namespace:(Some namespace) ~name:pod
        ~subresource
    with
    | Ok value -> Ok value
    | Error message -> Error (Protocol_error message)
  in
  let query =
    List.map (fun value -> ("command", value)) command
    @ Option.fold ~none:[]
        ~some:(fun value -> [ ("container", value) ])
        container
    @ [
        ("stdin", bool stdin);
        ("stdout", bool stdout);
        ("stderr", bool stderr);
        ("tty", bool tty);
      ]
  in
  let target =
    Uri.of_string path |> fun uri -> Uri.with_query' uri query |> Uri.to_string
  in
  match
    Client.websocket ?cancel client target
      ~protocols:(List.map protocol_name protocols)
  with
  | Error error -> Error (Client_error error)
  | Ok socket -> (
      match protocol_of_name (Websocket.protocol socket) with
      | Error error ->
          Websocket.close socket;
          Error error
      | Ok protocol ->
          Ok
            {
              socket;
              protocol;
              stdin;
              stdout;
              stderr;
              tty;
              stdin_closed = Atomic.make false;
            })

let exec ?cancel ?namespace ?container ?stdin ?stdout ?stderr ?tty ?protocols
    client ~pod ~command () =
  if command = [] then Error (Protocol_error "exec command must not be empty")
  else
    connect ?cancel ?namespace ?container ?stdin ?stdout ?stderr ?tty ?protocols
      client ~pod ~subresource:"exec" ~command ()

let attach ?cancel ?namespace ?container ?stdin ?stdout ?stderr ?tty ?protocols
    client ~pod () =
  connect ?cancel ?namespace ?container ?stdin ?stdout ?stderr ?tty ?protocols
    client ~pod ~subresource:"attach" ~command:[] ()

let channel channel payload =
  let output = Bytes.create (1 + String.length payload) in
  Bytes.set output 0 (Char.chr channel);
  Bytes.blit_string payload 0 output 1 (String.length payload);
  Bytes.unsafe_to_string output

let websocket_result = function
  | Ok value -> Ok value
  | Error message -> Error (Protocol_error message)

let send_stdin session payload =
  if not session.stdin then Error (Protocol_error "stdin was not requested")
  else if Atomic.get session.stdin_closed then
    Error (Protocol_error "stdin is already closed")
  else
    Websocket.send_binary session.socket (channel 0 payload) |> websocket_result

let close_stdin session =
  if not session.stdin then Error (Protocol_error "stdin was not requested")
  else if session.protocol <> V5 then
    Error (Protocol_error "stdin half-close requires stream protocol V5")
  else if Atomic.compare_and_set session.stdin_closed false true then
    Websocket.send_binary session.socket (channel 255 "\000")
    |> websocket_result
  else Ok ()

let resize session ~width ~height =
  if not session.tty then
    Error (Protocol_error "terminal resize requires a TTY")
  else if protocol_number session.protocol < 3 then
    Error
      (Protocol_error "terminal resize requires stream protocol V3 or newer")
  else if width <= 0 || width > 65535 || height <= 0 || height > 65535 then
    Error
      (Protocol_error "terminal width and height must be between 1 and 65535")
  else
    let payload =
      `Assoc [ ("Width", `Int width); ("Height", `Int height) ]
      |> Yojson.Safe.to_string
    in
    Websocket.send_binary session.socket (channel 4 payload) |> websocket_result

let member name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let string = function
  | `String value -> Some value
  | _ -> None

let integer = function
  | `Int value -> Some value
  | `Intlit value -> int_of_string_opt value
  | _ -> None

let status_of_json json =
  let status = Option.bind (member "status" json) string in
  let reason = Option.bind (member "reason" json) string in
  let message =
    Option.bind (member "message" json) string |> Option.value ~default:""
  in
  let code = Option.bind (member "code" json) integer in
  { status; reason; message; code; body = Some json }

let exit_code json =
  match Option.bind (member "details" json) (member "causes") with
  | Some (`List causes) ->
      List.find_map
        (function
          | `Assoc fields -> (
              match
                ( Option.bind (List.assoc_opt "type" fields) string,
                  Option.bind (List.assoc_opt "message" fields) string )
              with
              | Some "ExitCode", Some value -> (
                  match int_of_string_opt value with
                  | Some code when code >= 0 && code <= 255 -> Some code
                  | Some _ | None -> None)
              | _ -> None)
          | _ -> None)
        causes
  | Some _ | None -> None

let error_event session payload =
  if protocol_number session.protocol < 4 then
    Ok
      (Remote_error
         {
           status = Some "Failure";
           reason = None;
           message = payload;
           code = None;
           body = None;
         })
  else
    try
      let json = Yojson.Safe.from_string payload in
      let status = status_of_json json in
      match (status.status, status.reason) with
      | Some "Success", _ -> Ok (Exit Success)
      | Some "Failure", Some "NonZeroExitCode" -> (
          match exit_code json with
          | Some code -> Ok (Exit (Exit_code code))
          | None ->
              Error
                (Protocol_error
                   "NonZeroExitCode Status has no valid ExitCode cause"))
      | _ -> Ok (Remote_error status)
    with Yojson.Json_error message ->
      Error (Protocol_error ("invalid Status on error stream: " ^ message))

let stream_of_id = function
  | 0 -> Some Stdin
  | 1 -> Some Stdout
  | 2 -> Some Stderr
  | 3 -> Some Error_stream
  | 4 -> Some Resize
  | _ -> None

let receive session =
  match Websocket.receive session.socket with
  | Error message -> Error (Protocol_error message)
  | Ok (Websocket.Close close) -> Ok (Connection_closed close)
  | Ok (Websocket.Text _) ->
      Error (Protocol_error "remote-command message must be binary")
  | Ok (Websocket.Binary "") ->
      Error (Protocol_error "remote-command message has no channel byte")
  | Ok (Websocket.Binary value) -> (
      let channel_id = Char.code value.[0] in
      let payload = String.sub value 1 (String.length value - 1) in
      match channel_id with
      | 1 when session.stdout -> Ok (Stdout_data payload)
      | 2 when session.stderr -> Ok (Stderr_data payload)
      | 3 -> error_event session payload
      | 255 when session.protocol = V5 -> (
          if String.length payload <> 1 then
            Error
              (Protocol_error "V5 CLOSE signal must name exactly one stream")
          else
            match stream_of_id (Char.code payload.[0]) with
            | Some stream -> Ok (Stream_closed stream)
            | None ->
                Error (Protocol_error "V5 CLOSE signal names an unknown stream")
          )
      | 0 -> Error (Protocol_error "server sent data on the stdin channel")
      | 1 -> Error (Protocol_error "server sent unrequested stdout data")
      | 2 -> Error (Protocol_error "server sent unrequested stderr data")
      | 4 -> Error (Protocol_error "server sent data on the resize channel")
      | 255 -> Error (Protocol_error "CLOSE signal requires stream protocol V5")
      | _ -> Error (Protocol_error "server sent an unknown stream channel"))

let close session = Websocket.close session.socket
