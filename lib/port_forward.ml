type error =
  | Client_error of Client.error
  | Protocol_error of string
  | Io_error of string

let ( let* ) result fn =
  match result with
  | Ok value -> fn value
  | Error _ as error -> error

let pp_error formatter = function
  | Client_error error -> Client.pp_error formatter error
  | Protocol_error message ->
      Format.fprintf formatter "port-forward protocol error: %s" message
  | Io_error message ->
      Format.fprintf formatter "port-forward I/O error: %s" message

let protocol = "SPDY/3.1+portforward.k8s.io"

let pod_api =
  {
    Core.group = "";
    version = "v1";
    kind = "Pod";
    plural = "pods";
    scope = Core.Namespaced;
  }

let namespace client = function
  | Some namespace -> namespace
  | None ->
      Option.value ~default:"default" (Client.config client).Config.namespace

let validate_name field value =
  if value = "" then Error (Protocol_error (field ^ " must not be empty"))
  else Ok ()

let validate_remote_port port =
  if port < 1 || port > 65535 then
    Error (Protocol_error "remote port must be between 1 and 65535")
  else Ok ()

type t = { tunnel : Spdy.t; request_id : int Atomic.t; closed : bool Atomic.t }
type error_state = Pending | Complete of string

type stream = {
  data : Spdy.stream;
  error : Spdy.stream;
  error_lock : Mutex.t;
  error_changed : Condition.t;
  mutable error_state : error_state;
  mutable error_thread : Thread.t option;
  closed : bool Atomic.t;
}

let connect ?cancel ?namespace:requested_namespace ?(ports = [])
    ?(max_stream_buffer_bytes = 4 * 1024 * 1024) client ~pod () =
  let* () = validate_name "pod" pod in
  let namespace = namespace client requested_namespace in
  let* () = validate_name "namespace" namespace in
  let rec validate = function
    | [] -> Ok ()
    | port :: rest ->
        let* () = validate_remote_port port in
        validate rest
  in
  let* () = validate ports in
  let* path =
    match
      Core.subresource_path pod_api ~namespace:(Some namespace) ~name:pod
        ~subresource:"portforward"
    with
    | Ok value -> Ok value
    | Error message -> Error (Protocol_error message)
  in
  let query = List.map (fun port -> ("port", string_of_int port)) ports in
  let target =
    Uri.of_string path |> fun uri -> Uri.with_query' uri query |> Uri.to_string
  in
  match Client.websocket ?cancel client target ~protocols:[ protocol ] with
  | Error error -> Error (Client_error error)
  | Ok socket -> (
      if Websocket.protocol socket <> Some protocol then (
        Websocket.close socket;
        Error
          (Protocol_error
             "API server did not negotiate the Kubernetes port-forward protocol"))
      else
        match Spdy.create ~max_stream_buffer_bytes socket with
        | Error message ->
            Websocket.close socket;
            Error (Protocol_error message)
        | Ok tunnel ->
            Ok
              { tunnel; request_id = Atomic.make 0; closed = Atomic.make false }
      )

let next_request_id connection =
  let rec loop () =
    let current = Atomic.get connection.request_id in
    if current = max_int then
      Error "port-forward request identifiers are exhausted"
    else if Atomic.compare_and_set connection.request_id current (current + 1)
    then Ok current
    else loop ()
  in
  loop ()

let headers ~stream_type ~port ~request_id =
  [
    ("streamType", stream_type);
    ("port", string_of_int port);
    ("requestID", string_of_int request_id);
  ]

let collect_error stream =
  let buffer = Bytes.create 4096 in
  let output = Buffer.create 128 in
  let rec loop () =
    match Spdy.read stream.error buffer 0 (Bytes.length buffer) with
    | Ok 0 -> Buffer.contents output
    | Ok count ->
        if Buffer.length output > (1024 * 1024) - count then
          "remote error stream exceeded 1 MiB"
        else (
          Buffer.add_subbytes output buffer 0 count;
          loop ())
    | Error message -> message
  in
  let message = loop () in
  Mutex.lock stream.error_lock;
  stream.error_state <- Complete message;
  Condition.broadcast stream.error_changed;
  Mutex.unlock stream.error_lock;
  if message <> "" && not (Atomic.get stream.closed) then Spdy.reset stream.data

let open_stream ?timeout (connection : t) ~port =
  let* () = validate_remote_port port in
  if Atomic.get connection.closed then
    Error (Protocol_error "port-forward connection is closed")
  else
    let* request_id =
      next_request_id connection
      |> Result.map_error (fun value -> Protocol_error value)
    in
    match
      Spdy.create_stream ?timeout connection.tunnel
        (headers ~stream_type:"error" ~port ~request_id)
    with
    | Error message -> Error (Protocol_error message)
    | Ok error -> (
        match Spdy.close_write error with
        | Error message ->
            Spdy.reset error;
            Error (Protocol_error message)
        | Ok () -> (
            match
              Spdy.create_stream ?timeout connection.tunnel
                (headers ~stream_type:"data" ~port ~request_id)
            with
            | Error message ->
                Spdy.reset error;
                Error (Protocol_error message)
            | Ok data ->
                let stream =
                  {
                    data;
                    error;
                    error_lock = Mutex.create ();
                    error_changed = Condition.create ();
                    error_state = Pending;
                    error_thread = None;
                    closed = Atomic.make false;
                  }
                in
                stream.error_thread <- Some (Thread.create collect_error stream);
                Ok stream))

let current_remote_error stream =
  Mutex.lock stream.error_lock;
  let result =
    match stream.error_state with
    | Complete message when message <> "" -> Some message
    | Pending | Complete _ -> None
  in
  Mutex.unlock stream.error_lock;
  result

let await_remote_error stream =
  Mutex.lock stream.error_lock;
  while stream.error_state = Pending do
    Condition.wait stream.error_changed stream.error_lock
  done;
  let result =
    match stream.error_state with
    | Complete "" -> Ok ()
    | Complete message -> Error (Protocol_error message)
    | Pending -> assert false
  in
  Mutex.unlock stream.error_lock;
  result

let read stream buffer offset length =
  match current_remote_error stream with
  | Some message -> Error (Protocol_error message)
  | None -> (
      match Spdy.read stream.data buffer offset length with
      | Error message -> (
          match current_remote_error stream with
          | Some remote -> Error (Protocol_error remote)
          | None -> Error (Protocol_error message))
      | Ok 0 ->
          let* () = await_remote_error stream in
          Ok 0
      | Ok count -> Ok count)

let write stream value =
  match current_remote_error stream with
  | Some message -> Error (Protocol_error message)
  | None ->
      Spdy.write stream.data value
      |> Result.map_error (fun value -> Protocol_error value)

let close_write stream =
  Spdy.close_write stream.data
  |> Result.map_error (fun value -> Protocol_error value)

let close_stream stream =
  if Atomic.compare_and_set stream.closed false true then (
    Spdy.reset stream.data;
    Spdy.reset stream.error;
    match stream.error_thread with
    | Some thread when Thread.id thread <> Thread.id (Thread.self ()) ->
        Thread.join thread
    | Some _ | None -> ())

let close (connection : t) =
  if Atomic.compare_and_set connection.closed false true then
    Spdy.close connection.tunnel

module Forwarder = struct
  type connection = t
  type mapping = { local_port : int; remote_port : int }
  type bound_port = { address : string; local_port : int; remote_port : int }

  type listener = {
    descriptor : Unix.file_descr;
    remote_port : int;
    address : string;
    local_port : int;
  }

  type t = {
    connection : connection;
    listeners : listener list;
    cancel : Cancel.t;
    unlink_parent : unit -> unit;
    on_connection_error : error -> unit;
    stopped : bool Atomic.t;
    lock : Mutex.t;
    changed : Condition.t;
    mutable failure : error option;
    mutable active_descriptors : Unix.file_descr list;
    mutable active_handlers : int;
    mutable accept_threads : Thread.t list;
  }

  let connection forwarder = forwarder.connection

  let bound_ports forwarder =
    List.map
      (fun listener ->
        {
          address = listener.address;
          local_port = listener.local_port;
          remote_port = listener.remote_port;
        })
      forwarder.listeners

  let close_descriptor descriptor =
    try Unix.close descriptor with Unix.Unix_error _ -> ()

  let shutdown_descriptor descriptor =
    try Unix.shutdown descriptor Unix.SHUTDOWN_ALL
    with Unix.Unix_error _ -> ()

  let request_stop ?failure forwarder =
    Mutex.lock forwarder.lock;
    let was_stopped = Atomic.get forwarder.stopped in
    (match (was_stopped, forwarder.failure, failure) with
    | false, None, Some error -> forwarder.failure <- Some error
    | _ -> ());
    let first =
      (not was_stopped) && Atomic.compare_and_set forwarder.stopped false true
    in
    let active = if first then forwarder.active_descriptors else [] in
    Condition.broadcast forwarder.changed;
    Mutex.unlock forwarder.lock;
    if first then (
      List.iter
        (fun listener -> close_descriptor listener.descriptor)
        forwarder.listeners;
      List.iter shutdown_descriptor active;
      close forwarder.connection;
      Cancel.cancel forwarder.cancel)

  let write_all descriptor buffer count =
    let rec loop offset =
      if offset = count then Ok ()
      else
        try
          let written = Unix.write descriptor buffer offset (count - offset) in
          if written = 0 then
            Error (Io_error "local socket write returned zero")
          else loop (offset + written)
        with Unix.Unix_error (error, operation, _) ->
          Error (Io_error (operation ^ ": " ^ Unix.error_message error))
    in
    loop 0

  let forward_connection forwarder descriptor remote_port =
    let report error = try forwarder.on_connection_error error with _ -> () in
    match open_stream forwarder.connection ~port:remote_port with
    | Error error ->
        report error;
        request_stop ~failure:error forwarder
    | Ok stream -> (
        let upload_error = Atomic.make None in
        let stop_upload = Atomic.make false in
        let upload =
          Thread.create
            (fun () ->
              let buffer = Bytes.create 16384 in
              let rec loop () =
                try
                  match Unix.read descriptor buffer 0 (Bytes.length buffer) with
                  | 0 -> (
                      match close_write stream with
                      | Ok () -> ()
                      | Error error -> Atomic.set upload_error (Some error))
                  | count -> (
                      match write stream (Bytes.sub_string buffer 0 count) with
                      | Ok () -> loop ()
                      | Error error -> Atomic.set upload_error (Some error))
                with Unix.Unix_error (error, operation, _) ->
                  if
                    (not (Atomic.get forwarder.stopped))
                    && not (Atomic.get stop_upload)
                  then
                    Atomic.set upload_error
                      (Some
                         (Io_error (operation ^ ": " ^ Unix.error_message error)))
              in
              loop ())
            ()
        in
        let download_result =
          let buffer = Bytes.create 16384 in
          let rec loop () =
            match read stream buffer 0 (Bytes.length buffer) with
            | Error _ as error -> error
            | Ok 0 -> Ok ()
            | Ok count ->
                let* () = write_all descriptor buffer count in
                loop ()
          in
          loop ()
        in
        Atomic.set stop_upload true;
        shutdown_descriptor descriptor;
        close_stream stream;
        Thread.join upload;
        match (download_result, Atomic.get upload_error) with
        | Error error, _ | Ok (), Some error -> report error
        | Ok (), None -> ())

  let finish_handler forwarder descriptor =
    Mutex.lock forwarder.lock;
    forwarder.active_descriptors <-
      List.filter
        (fun candidate -> candidate <> descriptor)
        forwarder.active_descriptors;
    forwarder.active_handlers <- forwarder.active_handlers - 1;
    Condition.broadcast forwarder.changed;
    Mutex.unlock forwarder.lock;
    close_descriptor descriptor

  let start_handler forwarder descriptor remote_port =
    Mutex.lock forwarder.lock;
    let stopped = Atomic.get forwarder.stopped in
    if not stopped then (
      forwarder.active_descriptors <- descriptor :: forwarder.active_descriptors;
      forwarder.active_handlers <- forwarder.active_handlers + 1);
    Mutex.unlock forwarder.lock;
    if stopped then close_descriptor descriptor
    else
      try
        ignore
          (Thread.create
             (fun () ->
               Fun.protect
                 ~finally:(fun () -> finish_handler forwarder descriptor)
                 (fun () ->
                   try forward_connection forwarder descriptor remote_port
                   with exn -> (
                     let error =
                       Io_error
                         ("forwarding worker failed: " ^ Printexc.to_string exn)
                     in
                     try forwarder.on_connection_error error with _ -> ())))
             ())
      with exn ->
        finish_handler forwarder descriptor;
        request_stop
          ~failure:
            (Io_error
               ("cannot start forwarding worker: " ^ Printexc.to_string exn))
          forwarder

  let accept_loop forwarder listener =
    let rec loop () =
      if not (Atomic.get forwarder.stopped) then
        try
          let descriptor, _ = Unix.accept ~cloexec:true listener.descriptor in
          start_handler forwarder descriptor listener.remote_port;
          loop ()
        with
        | Unix.Unix_error ((Unix.EBADF | Unix.EINVAL), _, _)
          when Atomic.get forwarder.stopped -> ()
        | Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
        | Unix.Unix_error (error, operation, _) ->
            request_stop
              ~failure:(Io_error (operation ^ ": " ^ Unix.error_message error))
              forwarder
    in
    loop ()

  let validate_mapping (mapping : mapping) =
    if mapping.local_port < 0 || mapping.local_port > 65535 then
      Error (Protocol_error "local port must be between 0 and 65535")
    else validate_remote_port mapping.remote_port

  let inet_address value =
    try Ok (Unix.inet_addr_of_string value)
    with Failure _ ->
      Error
        (Protocol_error
           (Printf.sprintf "forward address %S is not an IP literal" value))

  let bind_listener address (mapping : mapping) =
    let* inet = inet_address address in
    let domain =
      if String.contains address ':' then Unix.PF_INET6 else Unix.PF_INET
    in
    let descriptor = Unix.socket ~cloexec:true domain Unix.SOCK_STREAM 0 in
    try
      Unix.setsockopt descriptor Unix.SO_REUSEADDR true;
      Unix.bind descriptor (Unix.ADDR_INET (inet, mapping.local_port));
      Unix.listen descriptor 128;
      let local_port =
        match Unix.getsockname descriptor with
        | Unix.ADDR_INET (_, port) -> port
        | Unix.ADDR_UNIX _ -> assert false
      in
      Ok { descriptor; remote_port = mapping.remote_port; address; local_port }
    with Unix.Unix_error (error, operation, _) ->
      close_descriptor descriptor;
      Error (Io_error (operation ^ ": " ^ Unix.error_message error))

  let close_listeners listeners =
    List.iter (fun listener -> close_descriptor listener.descriptor) listeners

  let start ?cancel:parent ?namespace ?(addresses = [ "127.0.0.1" ])
      ?max_stream_buffer_bytes ?(on_connection_error = fun _ -> ()) client ~pod
      ~(ports : mapping list) () =
    if ports = [] then
      Error (Protocol_error "at least one port mapping is required")
    else if addresses = [] then
      Error (Protocol_error "at least one listen address is required")
    else
      let rec validate_all = function
        | [] -> Ok ()
        | mapping :: rest ->
            let* () = validate_mapping mapping in
            validate_all rest
      in
      let* () = validate_all ports in
      let pairs =
        List.concat_map
          (fun mapping ->
            List.map (fun address -> (address, mapping)) addresses)
          ports
      in
      let rec bind_pairs listeners = function
        | [] -> Ok (List.rev listeners)
        | (address, mapping) :: rest -> (
            match bind_listener address mapping with
            | Ok listener -> bind_pairs (listener :: listeners) rest
            | Error error ->
                close_listeners listeners;
                Error error)
      in
      let* listeners = bind_pairs [] pairs in
      let cancel = Cancel.create () in
      let unlink_parent =
        match parent with
        | None -> Fun.id
        | Some parent ->
            Cancel.on_cancel parent (fun () -> Cancel.cancel cancel)
      in
      match
        connect ~cancel ?namespace
          ~ports:
            (List.map (fun (mapping : mapping) -> mapping.remote_port) ports)
          ?max_stream_buffer_bytes client ~pod ()
      with
      | Error error ->
          unlink_parent ();
          Cancel.cancel cancel;
          close_listeners listeners;
          Error error
      | Ok connection -> (
          let forwarder =
            {
              connection;
              listeners;
              cancel;
              unlink_parent;
              on_connection_error;
              stopped = Atomic.make false;
              lock = Mutex.create ();
              changed = Condition.create ();
              failure = None;
              active_descriptors = [];
              active_handlers = 0;
              accept_threads = [];
            }
          in
          let rec spawn = function
            | [] -> Ok ()
            | listener :: rest -> (
                try
                  let thread =
                    Thread.create (fun () -> accept_loop forwarder listener) ()
                  in
                  forwarder.accept_threads <- thread :: forwarder.accept_threads;
                  spawn rest
                with exn ->
                  Error
                    (Io_error
                       ("cannot start port-forward listener: "
                      ^ Printexc.to_string exn)))
          in
          match spawn listeners with
          | Error error ->
              request_stop ~failure:error forwarder;
              List.iter Thread.join forwarder.accept_threads;
              unlink_parent ();
              Error error
          | Ok () ->
              let _unlink_stop =
                Cancel.on_cancel cancel (fun () -> request_stop forwarder)
              in
              Ok forwarder)

  let await forwarder =
    Mutex.lock forwarder.lock;
    while not (Atomic.get forwarder.stopped) do
      Condition.wait forwarder.changed forwarder.lock
    done;
    let result =
      match forwarder.failure with
      | None -> Ok ()
      | Some error -> Error error
    in
    Mutex.unlock forwarder.lock;
    result

  let close forwarder =
    request_stop forwarder;
    forwarder.unlink_parent ();
    List.iter
      (fun thread ->
        if Thread.id thread <> Thread.id (Thread.self ()) then
          Thread.join thread)
      forwarder.accept_threads;
    Mutex.lock forwarder.lock;
    while forwarder.active_handlers > 0 do
      Condition.wait forwarder.changed forwarder.lock
    done;
    Mutex.unlock forwarder.lock
end

module For_testing = struct
  type peer = Spdy.For_testing.peer

  let peer = Spdy.For_testing.peer
  let decode_syn_stream = Spdy.For_testing.decode_syn_stream
  let syn_reply = Spdy.For_testing.syn_reply
  let data = Spdy.For_testing.data
  let decode_data = Spdy.For_testing.decode_data
end
