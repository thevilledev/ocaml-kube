type response = { delay : float; fragments : string list }

type t = {
  config : Kube.Config.t;
  requests_lock : Mutex.t;
  mutable requests : string list;
}

let write_all descriptor value =
  let rec loop offset =
    if offset < String.length value then
      let written =
        Unix.write_substring descriptor value offset
          (String.length value - offset)
      in
      if written = 0 then raise End_of_file else loop (offset + written)
  in
  loop 0

let response_head ~status headers =
  let output = Buffer.create 256 in
  Buffer.add_string output ("HTTP/1.1 " ^ status ^ "\r\n");
  List.iter
    (fun (name, value) ->
      Buffer.add_string output name;
      Buffer.add_string output ": ";
      Buffer.add_string output value;
      Buffer.add_string output "\r\n")
    headers;
  Buffer.add_string output "\r\n";
  Buffer.contents output

let fixed ?(status = "200 OK") ?(headers = []) body =
  {
    delay = 0.;
    fragments =
      [
        response_head ~status
          (("Content-Length", string_of_int (String.length body))
          :: ("Connection", "close") :: headers);
        body;
      ];
  }

let chunk value = Printf.sprintf "%x\r\n%s\r\n" (String.length value) value

let chunked ?(status = "200 OK") ?(headers = []) ?(trailers = [])
    ?(terminate = true) chunks =
  let ending =
    if not terminate then []
    else
      [
        "0\r\n"
        ^ String.concat ""
            (List.map
               (fun (name, value) -> name ^ ": " ^ value ^ "\r\n")
               trailers)
        ^ "\r\n";
      ]
  in
  {
    delay = 0.;
    fragments =
      response_head ~status
        (("Transfer-Encoding", "chunked") :: ("Connection", "close") :: headers)
      :: List.map chunk chunks
      @ ending;
  }

let raw fragments = { delay = 0.; fragments }

let delayed delay response =
  if (not (Float.is_finite delay)) || delay < 0. then
    invalid_arg "Fake_api_server.delayed: delay must be finite and non-negative";
  { response with delay }

let read_request descriptor =
  let output = Buffer.create 1024 in
  let buffer = Bytes.create 4096 in
  let rec boundary value index =
    if index + 4 > String.length value then None
    else if String.sub value index 4 = "\r\n\r\n" then Some index
    else boundary value (index + 1)
  in
  let content_length head =
    String.split_on_char '\n' head
    |> List.find_map (fun line ->
        match String.split_on_char ':' line with
        | name :: values
          when String.lowercase_ascii (String.trim name) = "content-length" ->
            String.concat ":" values |> String.trim |> int_of_string_opt
        | _ -> None)
    |> Option.value ~default:0
  in
  let rec loop () =
    let contents = Buffer.contents output in
    match boundary contents 0 with
    | Some ending ->
        let required =
          ending + 4 + content_length (String.sub contents 0 ending)
        in
        if String.length contents >= required then
          String.sub contents 0 required
        else read_more ()
    | None -> read_more ()
  and read_more () =
    let count = Unix.read descriptor buffer 0 (Bytes.length buffer) in
    if count = 0 then Buffer.contents output
    else (
      Buffer.add_subbytes output buffer 0 count;
      loop ())
  in
  loop ()

let config server = server.config

let requests server =
  Mutex.lock server.requests_lock;
  let result = List.rev server.requests in
  Mutex.unlock server.requests_lock;
  result

let with_server responses fn =
  let listener = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt listener Unix.SO_REUSEADDR true;
  Unix.bind listener (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen listener (max 1 (List.length responses));
  let port =
    match Unix.getsockname listener with
    | Unix.ADDR_INET (_, port) -> port
    | Unix.ADDR_UNIX _ -> assert false
  in
  let server =
    {
      config =
        Kube.Config.make
          (Uri.of_string (Printf.sprintf "http://127.0.0.1:%d" port));
      requests_lock = Mutex.create ();
      requests = [];
    }
  in
  let stopping = Atomic.make false in
  let server_error = Atomic.make None in
  let serve =
    Thread.create
      (fun () ->
        try
          List.iteri
            (fun index response ->
              let readable, _, _ = Unix.select [ listener ] [] [] 5.0 in
              if readable = [] then
                failwith
                  (Printf.sprintf
                     "timed out waiting for scripted API request %d of %d"
                     (index + 1) (List.length responses));
              let connection, _ = Unix.accept listener in
              Unix.setsockopt connection Unix.TCP_NODELAY true;
              Fun.protect
                ~finally:(fun () -> Unix.close connection)
                (fun () ->
                  let request = read_request connection in
                  Mutex.lock server.requests_lock;
                  server.requests <- request :: server.requests;
                  Mutex.unlock server.requests_lock;
                  if response.delay > 0. then
                    ignore (Unix.select [] [] [] response.delay);
                  List.iter (write_all connection) response.fragments))
            responses
        with exn ->
          if not (Atomic.get stopping) then
            Atomic.set server_error (Some (Printexc.to_string exn)))
      ()
  in
  let result =
    try Ok (fn server) with exn -> Error (exn, Printexc.get_raw_backtrace ())
  in
  Atomic.set stopping true;
  (try Unix.close listener with Unix.Unix_error _ -> ());
  Thread.join serve;
  match result with
  | Error (exn, backtrace) -> Printexc.raise_with_backtrace exn backtrace
  | Ok value -> (
      match Atomic.get server_error with
      | None -> value
      | Some message -> failwith ("scripted API server failed: " ^ message))
