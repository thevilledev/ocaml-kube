type event_type = Normal | Warning

type t = {
  client : Client.t;
  namespace : string option;
  reporting_controller : string;
  reporting_instance : string;
  instance_hash : string;
  now : unit -> Ptime.t;
}

let api =
  {
    Core.group = "events.k8s.io";
    version = "v1";
    kind = "Event";
    plural = "events";
    scope = Core.Namespaced;
  }

let create ?namespace ?(now = Ptime_clock.now) ~client ~reporting_controller
    ~reporting_instance () =
  if String.trim reporting_controller = "" then
    Error "reporting_controller must not be empty"
  else if String.trim reporting_instance = "" then
    Error "reporting_instance must not be empty"
  else if String.length reporting_instance > 128 then
    Error "reporting_instance must not exceed 128 bytes"
  else
    match namespace with
    | Some namespace when String.trim namespace = "" ->
        Error "namespace must not be empty"
    | None | Some _ ->
        let instance_hash =
          Digest.string reporting_instance |> Digest.to_hex |> fun value ->
          String.sub value 0 8
        in
        Ok
          {
            client;
            namespace;
            reporting_controller;
            reporting_instance;
            instance_hash;
            now;
          }

let alphanumeric = function
  | 'a' .. 'z' | '0' .. '9' -> true
  | _ -> false

let sanitize_name value =
  let value = String.lowercase_ascii value in
  let value =
    String.map
      (fun char ->
        if alphanumeric char || char = '-' || char = '.' then char else '-')
      value
  in
  let rec first index =
    if index >= String.length value then String.length value
    else if alphanumeric value.[index] then index
    else first (index + 1)
  in
  let rec last index =
    if index < 0 then -1
    else if alphanumeric value.[index] then index
    else last (index - 1)
  in
  let first = first 0 and last = last (String.length value - 1) in
  if first > last then "event" else String.sub value first (last - first + 1)

let next_event_id = Atomic.make 0

let event_name recorder (regarding : Core.object_reference) now =
  let microseconds = Int64.of_float (Ptime.to_float_s now *. 1_000_000.) in
  let sequence = Atomic.fetch_and_add next_event_id 1 in
  let suffix =
    Printf.sprintf "%Lx-%08x-%s" microseconds sequence recorder.instance_hash
  in
  let maximum_base = 253 - String.length suffix - 1 in
  let base = sanitize_name regarding.Core.name in
  let base =
    if String.length base <= maximum_base then base
    else String.sub base 0 maximum_base |> sanitize_name
  in
  base ^ "." ^ suffix

let empty_metadata ~namespace name =
  {
    Core.name;
    namespace = Some namespace;
    uid = None;
    resource_version = None;
    generation = None;
    deletion_timestamp = None;
    finalizers = [];
    owner_references = [];
    labels = [];
    annotations = [];
  }

let validate_field name maximum value =
  if String.trim value = "" then Error (name ^ " must not be empty")
  else if String.length value > maximum then
    Error (Printf.sprintf "%s must not exceed %d bytes" name maximum)
  else Ok ()

let validate_reference name (reference : Core.object_reference) =
  if String.trim reference.api_version = "" then
    Error (name ^ ".api_version must not be empty")
  else if String.trim reference.kind = "" then
    Error (name ^ ".kind must not be empty")
  else if String.trim reference.name = "" then
    Error (name ^ ".name must not be empty")
  else
    match reference.namespace with
    | Some namespace when String.trim namespace = "" ->
        Error (name ^ ".namespace must not be empty")
    | None | Some _ -> Ok ()

let event_namespace recorder (regarding : Core.object_reference) =
  match regarding.Core.namespace with
  | Some namespace -> namespace
  | None -> (
      match recorder.namespace with
      | Some namespace -> namespace
      | None ->
          Option.value ~default:"default"
            (Client.config recorder.client).Config.namespace)

let record ?cancel ?(related : Core.object_reference option) recorder
    ~(regarding : Core.object_reference) ~type_ ~reason ~action ~note =
  let ( let* ) result fn =
    match result with
    | Ok value -> fn value
    | Error _ as error -> error
  in
  let invalid message = Error (Client.Invalid_request message) in
  let* () =
    match validate_field "reason" 128 reason with
    | Ok () -> Ok ()
    | Error message -> invalid message
  in
  let* () =
    match validate_field "action" 128 action with
    | Ok () -> Ok ()
    | Error message -> invalid message
  in
  let* () =
    if String.length note <= 1024 then Ok ()
    else invalid "note must not exceed 1024 bytes"
  in
  let* () =
    match validate_reference "regarding" regarding with
    | Ok () -> Ok ()
    | Error message -> invalid message
  in
  let* () =
    match related with
    | None -> Ok ()
    | Some related -> (
        match validate_reference "related" related with
        | Ok () -> Ok ()
        | Error message -> invalid message)
  in
  let namespace = event_namespace recorder regarding in
  let now = recorder.now () in
  let metadata =
    empty_metadata ~namespace (event_name recorder regarding now)
  in
  let optional name fn = function
    | None -> []
    | Some value -> [ (name, fn value) ]
  in
  let value =
    `Assoc
      ([
         ("apiVersion", `String (Core.api_version api));
         ("kind", `String api.kind);
         ("metadata", Core.object_meta_to_json metadata);
         ("eventTime", `String (Ptime.to_rfc3339 ~frac_s:6 ~tz_offset_s:0 now));
         ("action", `String action);
         ("reason", `String reason);
         ("regarding", Core.object_reference_to_json regarding);
         ("note", `String note);
         ("reportingController", `String recorder.reporting_controller);
         ("reportingInstance", `String recorder.reporting_instance);
         ( "type",
           `String
             (match type_ with
             | Normal -> "Normal"
             | Warning -> "Warning") );
       ]
      @ optional "related" Core.object_reference_to_json related)
  in
  let event =
    {
      Dynamic.api_version = Core.api_version api;
      kind = api.kind;
      metadata;
      value;
    }
  in
  match Dynamic.create ?cancel recorder.client ~api ~namespace event with
  | Ok _ -> Ok ()
  | Error _ as error -> error
