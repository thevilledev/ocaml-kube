type cached_token = { token : string; expires_at : float option }

type exec = {
  api_version : string;
  command : string;
  args : string list;
  env : (string * string) list;
  provide_cluster_info : bool;
  cluster_info : Yojson.Safe.t;
  cache_mutex : Mutex.t;
  mutable cached_token : cached_token option;
}

type credential =
  | Anonymous
  | Static_token of string
  | Token_file of string
  | Basic of { username : string; password : string }
  | Exec of exec

type tls = {
  ca_pem : string option;
  client_certificate_pem : string option;
  client_key_pem : string option;
  insecure_skip_verify : bool;
  server_name : string option;
}

type impersonation = {
  user : string;
  uid : string option;
  groups : string list;
  extra : (string * string list) list;
}

type t = {
  server : Uri.t;
  namespace : string option;
  credential : credential;
  tls : tls;
  proxy_url : Uri.t option;
  impersonation : impersonation option;
}

type credential_origin = [ `Protected | `Heap ]

let default_tls =
  {
    ca_pem = None;
    client_certificate_pem = None;
    client_key_pem = None;
    insecure_skip_verify = false;
    server_name = None;
  }

let ( let* ) result fn =
  match result with
  | Ok value -> fn value
  | Error _ as error -> error

let read_file path =
  try
    let channel = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr channel)
      (fun () -> Ok (really_input_string channel (in_channel_length channel)))
  with Sys_error message -> Error message

let member name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let string name json =
  match member name json with
  | Some (`String value) -> Ok value
  | _ -> Error (name ^ " is required and must be a string")

let string_opt name json =
  match member name json with
  | Some (`String value) -> Some value
  | _ -> None

let bool_default name default json =
  match member name json with
  | Some (`Bool value) -> value
  | _ -> default

let strings name json =
  match member name json with
  | None -> Ok []
  | Some (`List values) ->
      let rec loop accumulator = function
        | [] -> Ok (List.rev accumulator)
        | `String value :: rest -> loop (value :: accumulator) rest
        | _ -> Error (name ^ " must contain only strings")
      in
      loop [] values
  | Some _ -> Error (name ^ " must be a list")

let string_lists name json =
  match member name json with
  | None -> Ok []
  | Some (`Assoc fields) ->
      let rec loop seen accumulator = function
        | [] -> Ok (List.rev accumulator)
        | (key, `List values) :: rest when not (List.mem key seen) ->
            let rec values_loop accumulator = function
              | [] -> Ok (List.rev accumulator)
              | `String value :: values ->
                  values_loop (value :: accumulator) values
              | _ -> Error (name ^ "." ^ key ^ " must contain only strings")
            in
            let* values = values_loop [] values in
            loop (key :: seen) ((key, values) :: accumulator) rest
        | (key, _) :: _ when List.mem key seen ->
            Error (name ^ " must not contain duplicate key " ^ key)
        | (key, _) :: _ -> Error (name ^ "." ^ key ^ " must be a list")
      in
      loop [] [] fields
  | Some _ -> Error (name ^ " must be an object")

let valid_header_value value =
  String.for_all
    (fun character ->
      let code = Char.code character in
      character = '\t' || (code >= 0x20 && code <> 0x7f))
    value

let make_impersonation ?uid ?(groups = []) ?(extra = []) ~user () =
  let user = String.trim user in
  if user = "" then Error "impersonated user must not be empty"
  else if not (valid_header_value user) then
    Error "impersonated user contains an invalid header value"
  else
    let uid =
      match Option.map String.trim uid with
      | Some "" | None -> None
      | Some value -> Some value
    in
    if
      Option.fold ~none:false
        ~some:(fun value -> not (valid_header_value value))
        uid
    then Error "impersonated UID contains an invalid header value"
    else if
      List.exists
        (fun value -> String.trim value = "" || not (valid_header_value value))
        groups
    then Error "impersonated groups must be non-empty valid header values"
    else
      let sorted = List.sort (fun (a, _) (b, _) -> String.compare a b) extra in
      let rec validate previous = function
        | [] -> Ok { user; uid; groups; extra = sorted }
        | (key, values) :: rest ->
            if key = "" then Error "impersonation extra keys must not be empty"
            else if String.lowercase_ascii key <> key then
              Error "impersonation extra keys must be lowercase"
            else if previous = Some key then
              Error ("duplicate impersonation extra key " ^ key)
            else if
              List.exists (fun value -> not (valid_header_value value)) values
            then Error ("impersonation extra " ^ key ^ " has an invalid value")
            else validate (Some key) rest
      in
      validate None sorted

let validate_server server =
  match (Uri.scheme server, Uri.host server) with
  | (Some "https" | Some "http"), Some _ -> Ok ()
  | _ -> Error ("invalid Kubernetes API server URL: " ^ Uri.to_string server)

let validate_proxy_url proxy =
  let path = Uri.path proxy in
  match
    (Option.map String.lowercase_ascii (Uri.scheme proxy), Uri.host proxy)
  with
  | Some ("http" | "https" | "socks5"), Some _
    when (path = "" || path = "/")
         && Uri.query proxy = []
         && Uri.fragment proxy = None -> Ok ()
  | Some ("http" | "https" | "socks5"), Some _ ->
      Error "proxy URL must not contain a path, query, or fragment"
  | Some scheme, _ -> Error ("unsupported proxy URL scheme: " ^ scheme)
  | None, _ -> Error "proxy URL must include a scheme"

let make ?namespace ?(credential = Anonymous) ?(tls = default_tls) ?proxy_url
    ?impersonation server =
  (match validate_server server with
  | Ok () -> ()
  | Error message -> invalid_arg message);
  Option.iter
    (fun proxy ->
      match validate_proxy_url proxy with
      | Ok () -> ()
      | Error message -> invalid_arg message)
    proxy_url;
  { server; namespace; credential; tls; proxy_url; impersonation }

let named name json =
  match json with
  | `List entries -> (
      List.find_map
        (function
          | `Assoc fields as entry -> (
              match List.assoc_opt "name" fields with
              | Some (`String value) when value = name -> Some entry
              | _ -> None)
          | _ -> None)
        entries
      |> function
      | Some value -> Ok value
      | None -> Error ("entry not found: " ^ name))
  | _ -> Error "expected a list of named entries"

let resolve_path ~base path =
  if Filename.is_relative path then Filename.concat base path else path

let decode_data field value =
  match Base64.decode value with
  | Ok value -> Ok (Some value)
  | Error (`Msg message) -> Error (field ^ ": invalid base64: " ^ message)

let data_or_file ~base ~data_field ~file_field json =
  match (string_opt data_field json, string_opt file_field json) with
  | Some value, _ -> decode_data data_field value
  | None, Some path ->
      let* value = read_file (resolve_path ~base path) in
      Ok (Some value)
  | None, None -> Ok None

let exec_environment json =
  match member "env" json with
  | None -> Ok []
  | Some (`List values) ->
      let rec loop accumulator = function
        | [] -> Ok (List.rev accumulator)
        | (`Assoc _ as value) :: rest ->
            let* name = string "name" value in
            let* contents = string "value" value in
            if name = "" then Error "exec.env variable names must not be empty"
            else loop ((name, contents) :: accumulator) rest
        | _ -> Error "exec.env entries must be objects with name and value"
      in
      loop [] values
  | Some _ -> Error "exec.env must be a list"

let cluster_info ~server ~ca_pem cluster =
  let optional name = function
    | None -> []
    | Some value -> [ (name, `String value) ]
  in
  `Assoc
    ([ ("server", `String server) ]
    @ (match ca_pem with
      | None -> []
      | Some value ->
          [
            ("certificate-authority-data", `String (Base64.encode_string value));
          ])
    @ (if bool_default "insecure-skip-tls-verify" false cluster then
         [ ("insecure-skip-tls-verify", `Bool true) ]
       else [])
    @ optional "tls-server-name" (string_opt "tls-server-name" cluster))

let exec_credential ~base ~server ~ca_pem cluster json =
  let* api_version = string "apiVersion" json in
  let* () =
    match api_version with
    | "client.authentication.k8s.io/v1" | "client.authentication.k8s.io/v1beta1"
      -> Ok ()
    | value -> Error ("unsupported exec credential apiVersion: " ^ value)
  in
  let* command = string "command" json in
  let* () =
    if command = "" then Error "exec credential command must not be empty"
    else Ok ()
  in
  let command =
    if
      Filename.is_relative command
      && (String.contains command '/'
         || (Sys.win32 && String.contains command '\\'))
    then resolve_path ~base command
    else command
  in
  let* args = strings "args" json in
  let* env = exec_environment json in
  let* interactive_mode =
    match string_opt "interactiveMode" json with
    | Some value -> Ok value
    | None when api_version = "client.authentication.k8s.io/v1beta1" ->
        Ok "IfAvailable"
    | None ->
        Error
          "exec credential interactiveMode is required for \
           client.authentication.k8s.io/v1"
  in
  let* () =
    match interactive_mode with
    | "Never" | "IfAvailable" -> Ok ()
    | "Always" ->
        Error "exec credential requires interactive input, which is unavailable"
    | value -> Error ("unsupported exec interactiveMode: " ^ value)
  in
  Ok
    {
      api_version;
      command;
      args;
      env;
      provide_cluster_info = bool_default "provideClusterInfo" false json;
      cluster_info = cluster_info ~server ~ca_pem cluster;
      cache_mutex = Mutex.create ();
      cached_token = None;
    }

let parse_document contents =
  let trimmed = String.trim contents in
  if trimmed <> "" && (trimmed.[0] = '{' || trimmed.[0] = '[') then
    try Ok (Yojson.Safe.from_string contents)
    with Yojson.Json_error message ->
      Error ("invalid kubeconfig JSON: " ^ message)
  else Yaml_lite.parse contents

let load_kubeconfigs ?context paths =
  let rec load_documents accumulator = function
    | [] -> Ok (List.rev accumulator)
    | "" :: rest -> load_documents accumulator rest
    | path :: rest ->
        let* contents =
          match read_file path with
          | Ok value -> Ok value
          | Error message -> Error (path ^ ": " ^ message)
        in
        let* document =
          match parse_document contents with
          | Ok value -> Ok value
          | Error message -> Error (path ^ ": " ^ message)
        in
        load_documents ((path, document) :: accumulator) rest
  in
  let* documents = load_documents [] paths in
  let* () =
    if documents = [] then Error "no kubeconfig paths were provided" else Ok ()
  in
  let rec find_named field name = function
    | [] -> Error (Printf.sprintf "%s entry not found: %s" field name)
    | (path, document) :: rest -> (
        match member field document with
        | None -> find_named field name rest
        | Some entries -> (
            match named name entries with
            | Ok entry -> Ok (Filename.dirname path, entry)
            | Error message
              when String.starts_with ~prefix:"entry not found:" message ->
                find_named field name rest
            | Error message -> Error (path ^ ": " ^ message)))
  in
  let rec first_current_context = function
    | [] -> Error "current-context is missing from all kubeconfig files"
    | (_, document) :: rest -> (
        match string_opt "current-context" document with
        | Some value when String.trim value <> "" -> Ok value
        | _ -> first_current_context rest)
  in
  let* context_name =
    match context with
    | Some value -> Ok value
    | None -> first_current_context documents
  in
  let* _context_base, context_entry =
    find_named "contexts" context_name documents
  in
  let* context_body =
    match member "context" context_entry with
    | Some value -> Ok value
    | None -> Error ("context body is missing for " ^ context_name)
  in
  let* cluster_name = string "cluster" context_body in
  let* cluster_base, cluster_entry =
    find_named "clusters" cluster_name documents
  in
  let* cluster =
    match member "cluster" cluster_entry with
    | Some value -> Ok value
    | None -> Error ("cluster body is missing for " ^ cluster_name)
  in
  let* server_string = string "server" cluster in
  let server = Uri.of_string server_string in
  let* () = validate_server server in
  let* proxy_url =
    match string_opt "proxy-url" cluster with
    | None -> Ok None
    | Some value when String.trim value = "" -> Ok None
    | Some value ->
        let value = Uri.of_string value in
        let* () = validate_proxy_url value in
        Ok (Some value)
  in
  let* ca_pem =
    data_or_file ~base:cluster_base ~data_field:"certificate-authority-data"
      ~file_field:"certificate-authority" cluster
  in
  let* user_base, user =
    match string_opt "user" context_body with
    | None -> Ok (cluster_base, `Assoc [])
    | Some user_name -> (
        let* base, entry = find_named "users" user_name documents in
        match member "user" entry with
        | Some value -> Ok (base, value)
        | None -> Error ("user body is missing for " ^ user_name))
  in
  let* client_certificate_pem =
    data_or_file ~base:user_base ~data_field:"client-certificate-data"
      ~file_field:"client-certificate" user
  in
  let* client_key_pem =
    data_or_file ~base:user_base ~data_field:"client-key-data"
      ~file_field:"client-key" user
  in
  let* credential =
    match
      ( string_opt "token" user,
        string_opt "tokenFile" user,
        string_opt "username" user,
        string_opt "password" user,
        member "exec" user )
    with
    | _, Some token_file, _, _, _ ->
        Ok (Token_file (resolve_path ~base:user_base token_file))
    | Some token, None, _, _, _ -> Ok (Static_token token)
    | None, None, Some username, Some password, _ ->
        Ok (Basic { username; password })
    | None, None, _, _, Some exec ->
        let* exec =
          exec_credential ~base:user_base ~server:server_string ~ca_pem cluster
            exec
        in
        Ok (Exec exec)
    | None, None, Some _, None, _ | None, None, None, Some _, _ ->
        Error
          "kubeconfig basic authentication requires both username and password"
    | None, None, None, None, None -> Ok Anonymous
  in
  let* groups = strings "as-groups" user in
  let* extra = string_lists "as-user-extra" user in
  let uid = string_opt "as-uid" user in
  let* impersonation =
    match string_opt "as" user with
    | Some value when String.trim value <> "" ->
        let* value = make_impersonation ?uid ~groups ~extra ~user:value () in
        Ok (Some value)
    | Some _ | None ->
        if uid <> None || groups <> [] || extra <> [] then
          Error
            "as-uid, as-groups, and as-user-extra require an impersonated user"
        else Ok None
  in
  Ok
    {
      server;
      namespace = string_opt "namespace" context_body;
      credential;
      proxy_url;
      impersonation;
      tls =
        {
          ca_pem;
          client_certificate_pem;
          client_key_pem;
          insecure_skip_verify =
            bool_default "insecure-skip-tls-verify" false cluster;
          server_name = string_opt "tls-server-name" cluster;
        };
    }

let load_kubeconfig ?context path = load_kubeconfigs ?context [ path ]
let service_account_directory = "/var/run/secrets/kubernetes.io/serviceaccount"

let in_cluster () =
  match Sys.getenv_opt "KUBERNETES_SERVICE_HOST" with
  | None -> Error "KUBERNETES_SERVICE_HOST is not set"
  | Some host ->
      let port =
        Option.value ~default:"443"
          (Sys.getenv_opt "KUBERNETES_SERVICE_PORT_HTTPS")
      in
      let host =
        if String.contains host ':' && not (String.starts_with ~prefix:"[" host)
        then "[" ^ host ^ "]"
        else host
      in
      let token_path = Filename.concat service_account_directory "token" in
      let ca_path = Filename.concat service_account_directory "ca.crt" in
      let namespace_path =
        Filename.concat service_account_directory "namespace"
      in
      let* ca_pem =
        if Sys.file_exists ca_path then
          let* pem = read_file ca_path in
          Ok (Some pem)
        else Ok None
      in
      let namespace =
        match read_file namespace_path with
        | Ok value ->
            let value = String.trim value in
            if value = "" then None else Some value
        | Error _ -> None
      in
      Ok
        {
          server = Uri.of_string ("https://" ^ host ^ ":" ^ port);
          namespace;
          credential = Token_file token_path;
          proxy_url = None;
          impersonation = None;
          tls =
            {
              ca_pem;
              client_certificate_pem = None;
              client_key_pem = None;
              insecure_skip_verify = false;
              server_name = None;
            };
        }

let default_kubeconfig_paths () =
  match Sys.getenv_opt "KUBECONFIG" with
  | Some value when String.trim value <> "" ->
      let separator = if Sys.win32 then ';' else ':' in
      String.split_on_char separator value
      |> List.filter (fun path -> path <> "")
  | None -> (
      match Sys.getenv_opt "HOME" with
      | Some directory -> [ Filename.concat directory ".kube/config" ]
      | None -> [ ".kube/config" ])
  | Some _ -> [ ".kube/config" ]

let load_default ?context () =
  match Sys.getenv_opt "KUBERNETES_SERVICE_HOST" with
  | Some _ -> in_cluster ()
  | None -> load_kubeconfigs ?context (default_kubeconfig_paths ())

let bearer_token config =
  match config.credential with
  | Anonymous -> Ok None
  | Static_token value -> Ok (Some (String.trim value))
  | Token_file path ->
      let* token = read_file path in
      Ok (Some (String.trim token))
  | Basic _ -> Ok None
  | Exec _ ->
      Error "exec credentials must be resolved through authorization_header"

let environment_with overrides =
  let overridden name =
    List.exists (fun (candidate, _) -> candidate = name) overrides
  in
  let inherited =
    Unix.environment () |> Array.to_list
    |> List.filter (fun entry ->
        match String.index_opt entry '=' with
        | None -> true
        | Some index ->
            let name = String.sub entry 0 index in
            not (overridden name))
  in
  Array.of_list
    (List.map (fun (name, value) -> name ^ "=" ^ value) overrides @ inherited)

let read_process_output ~timeout_seconds ~max_bytes command args environment =
  let stdout_read, stdout_write = Unix.pipe ~cloexec:true () in
  let stderr_read, stderr_write = Unix.pipe ~cloexec:true () in
  let dev_null = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
  let close_noerr descriptor =
    try Unix.close descriptor with Unix.Unix_error _ -> ()
  in
  let child = ref None in
  let terminate_child () =
    match !child with
    | None -> ()
    | Some pid ->
        (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
        (try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ());
        child := None
  in
  try
    let argv = Array.of_list (command :: args) in
    let pid =
      Unix.create_process_env command argv environment dev_null stdout_write
        stderr_write
    in
    child := Some pid;
    close_noerr dev_null;
    close_noerr stdout_write;
    close_noerr stderr_write;
    Unix.set_nonblock stdout_read;
    Unix.set_nonblock stderr_read;
    let stdout_buffer = Buffer.create 4096 in
    let stderr_buffer = Buffer.create 1024 in
    let stdout_open = ref true in
    let stderr_open = ref true in
    let status = ref None in
    let deadline = Clock.deadline timeout_seconds in
    let scratch = Bytes.create 8192 in
    let drain descriptor open_flag buffer =
      let rec loop () =
        try
          let count = Unix.read descriptor scratch 0 (Bytes.length scratch) in
          if count = 0 then open_flag := false
          else if Buffer.length buffer + count > max_bytes then
            raise
              (Failure
                 (Printf.sprintf "exec credential output exceeds %d bytes"
                    max_bytes))
          else (
            Buffer.add_subbytes buffer scratch 0 count;
            loop ())
        with Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> ()
      in
      loop ()
    in
    let rec loop () =
      if Clock.remaining deadline = 0. then (
        terminate_child ();
        raise
          (Failure
             (Printf.sprintf
                "exec credential command timed out after %.0f seconds"
                timeout_seconds)));
      (match !status with
      | Some _ -> ()
      | None -> (
          match Unix.waitpid [ Unix.WNOHANG ] pid with
          | 0, _ -> ()
          | _, child_status ->
              status := Some child_status;
              child := None));
      let readable =
        (if !stdout_open then [ stdout_read ] else [])
        @ if !stderr_open then [ stderr_read ] else []
      in
      if readable <> [] then (
        let ready, _, _ = Unix.select readable [] [] 0.1 in
        if List.mem stdout_read ready then
          drain stdout_read stdout_open stdout_buffer;
        if List.mem stderr_read ready then
          drain stderr_read stderr_open stderr_buffer);
      if !status = None || !stdout_open || !stderr_open then loop ()
    in
    loop ();
    close_noerr stdout_read;
    close_noerr stderr_read;
    match !status with
    | Some (Unix.WEXITED 0) -> Ok (Buffer.contents stdout_buffer)
    | Some (Unix.WEXITED code) ->
        Error
          (Printf.sprintf "exec credential command exited %d: %s" code
             (String.trim (Buffer.contents stderr_buffer)))
    | Some (Unix.WSIGNALED signal) ->
        Error
          (Printf.sprintf "exec credential command was killed by signal %d"
             signal)
    | Some (Unix.WSTOPPED signal) ->
        Error
          (Printf.sprintf "exec credential command stopped by signal %d" signal)
    | None -> Error "exec credential command did not return a status"
  with
  | Unix.Unix_error (code, fn, argument) ->
      terminate_child ();
      close_noerr dev_null;
      close_noerr stdout_read;
      close_noerr stdout_write;
      close_noerr stderr_read;
      close_noerr stderr_write;
      Error (Printf.sprintf "%s(%s): %s" fn argument (Unix.error_message code))
  | Failure message ->
      terminate_child ();
      close_noerr dev_null;
      close_noerr stdout_read;
      close_noerr stdout_write;
      close_noerr stderr_read;
      close_noerr stderr_write;
      Error message

let exec_info exec =
  let spec =
    [ ("interactive", `Bool false) ]
    @
    if exec.provide_cluster_info then [ ("cluster", exec.cluster_info) ] else []
  in
  Yojson.Safe.to_string
    (`Assoc
       [
         ("apiVersion", `String exec.api_version);
         ("kind", `String "ExecCredential");
         ("spec", `Assoc spec);
       ])

let parse_exec_token exec output =
  try
    let json = Yojson.Safe.from_string output in
    let* () =
      match string_opt "kind" json with
      | Some "ExecCredential" -> Ok ()
      | _ -> Error "exec credential output kind must be ExecCredential"
    in
    let* () =
      match string_opt "apiVersion" json with
      | Some value when value = exec.api_version -> Ok ()
      | Some value ->
          Error
            (Printf.sprintf
               "exec credential returned apiVersion %s, expected %s" value
               exec.api_version)
      | None -> Error "exec credential output has no apiVersion"
    in
    let* status =
      match member "status" json with
      | Some (`Assoc _ as status) -> Ok status
      | _ -> Error "exec credential output has no status object"
    in
    let* token =
      match string_opt "token" status with
      | Some token when String.trim token <> "" -> Ok (String.trim token)
      | _ ->
          Error
            "exec credential returned no token; certificate exec credentials \
             are not supported"
    in
    let expires_at =
      match string_opt "expirationTimestamp" status with
      | None -> Ok None
      | Some timestamp -> (
          match Ptime.of_rfc3339 timestamp with
          | Ok (time, _, _) -> Ok (Some (Ptime.to_float_s time))
          | Error _ ->
              Error "exec credential returned an invalid expirationTimestamp")
    in
    let* expires_at = expires_at in
    Ok { token; expires_at }
  with Yojson.Json_error message ->
    Error ("invalid exec credential JSON: " ^ message)

let exec_token exec =
  Mutex.lock exec.cache_mutex;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock exec.cache_mutex)
    (fun () ->
      let now = Unix.gettimeofday () in
      match exec.cached_token with
      | Some cached
        when Option.fold ~none:true
               ~some:(fun expires -> expires -. 30. > now)
               cached.expires_at -> Ok cached.token
      | _ ->
          let environment =
            environment_with
              (List.filter
                 (fun (name, _) -> name <> "KUBERNETES_EXEC_INFO")
                 exec.env
              @ [ ("KUBERNETES_EXEC_INFO", exec_info exec) ])
          in
          let* output =
            read_process_output ~timeout_seconds:30. ~max_bytes:(1024 * 1024)
              exec.command exec.args environment
          in
          let* cached = parse_exec_token exec output in
          exec.cached_token <- Some cached;
          Ok cached.token)

let authorization_header config =
  match config.credential with
  | Anonymous -> Ok None
  | Static_token value -> Ok (Some ("Bearer " ^ String.trim value))
  | Token_file path ->
      let* token = read_file path in
      Ok (Some ("Bearer " ^ String.trim token))
  | Basic { username; password } ->
      Ok (Some ("Basic " ^ Base64.encode_string (username ^ ":" ^ password)))
  | Exec exec ->
      let* token = exec_token exec in
      Ok (Some ("Bearer " ^ token))

let whitespace = function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false

let with_bearer_secret ?(hardened = false) token fn =
  let first, last =
    Secret.Unsafe.with_string_view token (fun value ->
        let first = ref 0 in
        let last = ref (String.length value) in
        while !first < !last && whitespace value.[!first] do
          incr first
        done;
        while !last > !first && whitespace value.[!last - 1] do
          decr last
        done;
        (!first, !last))
  in
  if first = last then Error "bearer token is empty"
  else
    let authorization = Secret.create ~hardened (7 + last - first) in
    Fun.protect
      ~finally:(fun () -> Secret.destroy authorization)
      (fun () ->
        Secret.blit_from_string "Bearer " ~src_off:0 authorization ~dst_off:0
          ~len:7;
        Secret.blit ~src:token ~src_off:first ~dst:authorization ~dst_off:7
          ~len:(last - first);
        Ok (fn authorization))

let with_heap_authorization ?(hardened = false) value fn =
  let authorization = Secret.of_string ~hardened value in
  Fun.protect
    ~finally:(fun () -> Secret.destroy authorization)
    (fun () -> Ok (fn authorization))

let with_authorization_secret ?(hardened = false) config fn =
  match config.credential with
  | Anonymous -> Ok (fn ~origin:`Protected None)
  | Token_file path -> (
      try
        let token = Secret_unix.read_file ~hardened ~max:(1024 * 1024) path in
        Fun.protect
          ~finally:(fun () -> Secret.destroy token)
          (fun () ->
            with_bearer_secret ~hardened token (fun authorization ->
                fn ~origin:`Protected (Some authorization)))
      with
      | Sys_error message -> Error message
      | Unix.Unix_error (code, name, argument) ->
          Error
            (Printf.sprintf "%s(%s): %s" name argument (Unix.error_message code))
      )
  | Static_token token ->
      let token_secret = Secret.of_string ~hardened token in
      Fun.protect
        ~finally:(fun () -> Secret.destroy token_secret)
        (fun () ->
          with_bearer_secret ~hardened token_secret (fun authorization ->
              fn ~origin:`Heap (Some authorization)))
  | Basic { username; password } ->
      let value = "Basic " ^ Base64.encode_string (username ^ ":" ^ password) in
      with_heap_authorization ~hardened value (fun authorization ->
          fn ~origin:`Heap (Some authorization))
  | Exec exec ->
      let* token = exec_token exec in
      let token_secret = Secret.of_string ~hardened token in
      Fun.protect
        ~finally:(fun () -> Secret.destroy token_secret)
        (fun () ->
          with_bearer_secret ~hardened token_secret (fun authorization ->
              fn ~origin:`Heap (Some authorization)))

let header_key_escape key =
  let legal = function
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '0' .. '9'
    | '!'
    | '#'
    | '$'
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
  let output = Buffer.create (String.length key) in
  String.iter
    (fun character ->
      if legal character && character <> '%' then
        Buffer.add_char output character
      else
        Buffer.add_string output (Printf.sprintf "%%%02X" (Char.code character)))
    key;
  Buffer.contents output

let impersonation_headers config =
  match config.impersonation with
  | None -> []
  | Some value ->
      [ ("Impersonate-User", value.user) ]
      @ Option.fold ~none:[]
          ~some:(fun uid -> [ ("Impersonate-Uid", uid) ])
          value.uid
      @ List.map (fun group -> ("Impersonate-Group", group)) value.groups
      @ List.concat_map
          (fun (key, values) ->
            List.map
              (fun value ->
                ("Impersonate-Extra-" ^ header_key_escape key, value))
              values)
          value.extra

let invalidate_credential config =
  match config.credential with
  | Exec exec ->
      Mutex.lock exec.cache_mutex;
      exec.cached_token <- None;
      Mutex.unlock exec.cache_mutex;
      true
  | _ -> false
