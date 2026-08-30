type status = Idle | Starting | Listening of int | Failed of string | Stopped

type t = {
  address : string;
  requested_port : int;
  health : Health.t;
  metrics : Metrics.t;
  lock : Mutex.t;
  changed : Condition.t;
  mutable status : status;
  mutable manager_component : Manager.component option;
}

type handler = { thread : Thread.t; finished : bool Atomic.t }

let create ?(address = "127.0.0.1") ?(port = 8080) ?health ?metrics () =
  if port < 0 || port > 65535 then
    invalid_arg "Diagnostics.create: invalid port";
  {
    address;
    requested_port = port;
    health = Option.value ~default:(Health.create ()) health;
    metrics = Option.value ~default:(Metrics.create ()) metrics;
    lock = Mutex.create ();
    changed = Condition.create ();
    status = Idle;
    manager_component = None;
  }

let health server = server.health
let metrics server = server.metrics

let protect server fn =
  Mutex.lock server.lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock server.lock) fn

let set_status server status =
  protect server (fun () ->
      server.status <- status;
      Condition.broadcast server.changed)

let write_all cancel fd value =
  let deadline = Clock.deadline 2. in
  let rec loop offset =
    if offset < String.length value then
      if Cancel.is_cancelled cancel || Clock.remaining deadline = 0. then
        raise End_of_file
      else
        try
          let count =
            Unix.write_substring fd value offset (String.length value - offset)
          in
          if count = 0 then raise End_of_file else loop (offset + count)
        with
        | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
            ignore (Unix.select [] [ fd ] [] 0.2);
            loop offset
        | Unix.Unix_error (Unix.EINTR, _, _) -> loop offset
  in
  loop 0

let find_boundary value =
  let rec loop index =
    if index + 4 > String.length value then None
    else if String.sub value index 4 = "\r\n\r\n" then Some index
    else loop (index + 1)
  in
  loop 0

let read_request cancel fd =
  let deadline = Clock.deadline 2. in
  let output = Buffer.create 1024 in
  let buffer = Bytes.create 1024 in
  let rec loop () =
    if Cancel.is_cancelled cancel then Error "server shutting down"
    else if Buffer.length output > 16 * 1024 then
      Error "request headers too large"
    else
      let contents = Buffer.contents output in
      match find_boundary contents with
      | Some boundary -> Ok (String.sub contents 0 boundary)
      | None -> (
          let remaining = Clock.remaining deadline in
          if remaining <= 0. then Error "request header timeout"
          else
            let readable, _, _ = Unix.select [ fd ] [] [] (min 0.2 remaining) in
            if readable = [] then loop ()
            else
              try
                let count = Unix.read fd buffer 0 (Bytes.length buffer) in
                if count = 0 then Error "connection closed before request"
                else (
                  Buffer.add_subbytes output buffer 0 count;
                  loop ())
              with
              | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _)
              | Unix.Unix_error (Unix.EINTR, _, _)
              ->
                loop ())
  in
  loop ()

let parse_request head =
  match String.split_on_char '\n' head with
  | [] -> Error "empty request"
  | line :: _ -> (
      match String.split_on_char ' ' (String.trim line) with
      | [ meth; target; protocol ]
        when String.starts_with ~prefix:"HTTP/" protocol -> Ok (meth, target)
      | _ -> Error "invalid request line")

let response ?(content_type = "text/plain; charset=utf-8") status reason body =
  Printf.sprintf
    "HTTP/1.1 %d %s\r\n\
     Content-Type: %s\r\n\
     Content-Length: %d\r\n\
     Cache-Control: no-store\r\n\
     Connection: close\r\n\
     \r\n\
     %s"
    status reason content_type (String.length body) body

let failures_body failures =
  let output = Buffer.create 128 in
  List.iter
    (fun failure ->
      Buffer.add_string output
        ("[-]" ^ failure.Health.check ^ " failed: " ^ failure.message ^ "\n"))
    failures;
  Buffer.contents output

let health_response check =
  match check () with
  | Ok () -> response 200 "OK" "ok\n"
  | Error failures ->
      response 503 "Service Unavailable" (failures_body failures)

let route server meth target =
  if meth <> "GET" then response 405 "Method Not Allowed" "method not allowed\n"
  else
    let path =
      match String.index_opt target '?' with
      | None -> target
      | Some index -> String.sub target 0 index
    in
    match path with
    | "/healthz" -> health_response (fun () -> Health.liveness server.health)
    | "/readyz" -> health_response (fun () -> Health.readiness server.health)
    | "/metrics" ->
        response ~content_type:Metrics.content_type 200 "OK"
          (Metrics.render server.metrics)
    | _ -> response 404 "Not Found" "not found\n"

let handle server cancel fd =
  Fun.protect
    ~finally:(fun () -> try Unix.close fd with Unix.Unix_error _ -> ())
    (fun () ->
      try
        Unix.set_nonblock fd;
        match read_request cancel fd with
        | Error message ->
            write_all cancel fd (response 400 "Bad Request" (message ^ "\n"))
        | Ok head -> (
            match parse_request head with
            | Error message ->
                write_all cancel fd
                  (response 400 "Bad Request" (message ^ "\n"))
            | Ok (meth, target) ->
                write_all cancel fd (route server meth target))
      with Unix.Unix_error _ | End_of_file -> ())

let reap handlers =
  let finished, running =
    List.partition (fun handler -> Atomic.get handler.finished) handlers
  in
  List.iter (fun handler -> Thread.join handler.thread) finished;
  running

let run server ~client ~cancel =
  let logger =
    Log.with_name (Client.logger client) "diagnostics" |> fun logger ->
    Log.with_fields logger
      [
        ("address", Log.String server.address);
        ("requested_port", Log.Int server.requested_port);
      ]
  in
  let started =
    protect server (fun () ->
        match server.status with
        | Idle ->
            server.status <- Starting;
            Condition.broadcast server.changed;
            true
        | Starting | Listening _ | Failed _ | Stopped -> false)
  in
  if not started then (
    Log.error logger "Diagnostics server was started more than once";
    Error (Client.Invalid_request "diagnostics server can only be started once"))
  else
    try
      let address = Unix.inet_addr_of_string server.address in
      let domain =
        if String.contains server.address ':' then Unix.PF_INET6
        else Unix.PF_INET
      in
      let listener = Unix.socket ~cloexec:true domain Unix.SOCK_STREAM 0 in
      Fun.protect
        ~finally:(fun () ->
          try Unix.close listener with Unix.Unix_error _ -> ())
        (fun () ->
          Unix.setsockopt listener Unix.SO_REUSEADDR true;
          Unix.bind listener (Unix.ADDR_INET (address, server.requested_port));
          Unix.listen listener 64;
          let port =
            match Unix.getsockname listener with
            | Unix.ADDR_INET (_, port) -> port
            | Unix.ADDR_UNIX _ -> assert false
          in
          set_status server (Listening port);
          Log.info logger
            ~fields:[ ("port", Log.Int port) ]
            "Diagnostics server listening";
          let handlers = ref [] in
          while not (Cancel.is_cancelled cancel) do
            handlers := reap !handlers;
            let readable, _, _ = Unix.select [ listener ] [] [] 0.2 in
            if readable <> [] then
              let fd, _ = Unix.accept ~cloexec:true listener in
              let finished = Atomic.make false in
              let thread =
                Thread.create
                  (fun () ->
                    Fun.protect
                      ~finally:(fun () -> Atomic.set finished true)
                      (fun () -> handle server cancel fd))
                  ()
              in
              handlers := { thread; finished } :: !handlers
          done;
          List.iter (fun handler -> Thread.join handler.thread) !handlers;
          set_status server Stopped;
          Log.info logger "Diagnostics server stopped";
          Ok ())
    with
    | Unix.Unix_error (code, fn, argument) ->
        let message =
          Printf.sprintf "%s(%s): %s" fn argument (Unix.error_message code)
        in
        set_status server (Failed message);
        Log.error logger
          ~fields:[ ("error", Log.String message) ]
          "Diagnostics server failed";
        Error (Client.Transport message)
    | exn ->
        let message = Printexc.to_string exn in
        set_status server (Failed message);
        Log.error logger
          ~fields:[ ("error", Log.String message) ]
          "Diagnostics server failed";
        Error (Client.Transport message)

let component server =
  protect server (fun () ->
      match server.manager_component with
      | Some component -> component
      | None ->
          let component =
            Manager.component
              ~name:
                (Printf.sprintf "diagnostics/%s:%d" server.address
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
            Error "diagnostics server stopped before listening")
  in
  unregister ();
  result
