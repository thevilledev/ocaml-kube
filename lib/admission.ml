type group_version_kind = { group : string; version : string; kind : string }

type group_version_resource = {
  group : string;
  version : string;
  resource : string;
}

type operation = Create | Update | Delete | Connect | Unknown of string

type user_info = {
  username : string;
  uid : string option;
  groups : string list;
  extra : (string * string list) list;
}

type request = {
  uid : string;
  kind : group_version_kind;
  resource : group_version_resource;
  subresource : string option;
  request_kind : group_version_kind option;
  request_resource : group_version_resource option;
  request_subresource : string option;
  name : string option;
  namespace : string option;
  operation : operation;
  user_info : user_info;
  object_ : Yojson.Safe.t option;
  old_object : Yojson.Safe.t option;
  dry_run : bool option;
  options : Yojson.Safe.t option;
}

type patch_operation =
  | Add of { path : string; value : Yojson.Safe.t }
  | Remove of { path : string }
  | Replace of { path : string; value : Yojson.Safe.t }
  | Move of { from : string; path : string }
  | Copy of { from : string; path : string }
  | Test of { path : string; value : Yojson.Safe.t }

type denial = { code : int; reason : string; message : string }
type decision = Allowed | Denied of denial | Patched of patch_operation list

type outcome = {
  decision : decision;
  warnings : string list;
  audit_annotations : (string * string) list;
}

type handler = cancel:Cancel.t -> request -> (outcome, string) result

let validate_metadata warnings audit_annotations =
  let seen = Hashtbl.create (List.length audit_annotations) in
  List.iter
    (fun (key, _) ->
      if String.trim key = "" then
        invalid_arg "Admission: empty audit annotation key";
      if Hashtbl.mem seen key then
        invalid_arg ("Admission: duplicate audit annotation key " ^ key);
      Hashtbl.add seen key ())
    audit_annotations;
  List.iter
    (fun warning ->
      if String.contains warning '\n' || String.contains warning '\r' then
        invalid_arg "Admission: warnings must be single-line strings")
    warnings

let outcome ?(warnings = []) ?(audit_annotations = []) decision =
  validate_metadata warnings audit_annotations;
  { decision; warnings; audit_annotations }

let allow ?warnings ?audit_annotations () =
  outcome ?warnings ?audit_annotations Allowed

let deny ?(code = 403) ?(reason = "Forbidden") ?warnings ?audit_annotations
    message =
  if code < 100 || code > 599 then invalid_arg "Admission.deny: invalid code";
  if String.trim reason = "" then invalid_arg "Admission.deny: empty reason";
  outcome ?warnings ?audit_annotations (Denied { code; reason; message })

let patch ?warnings ?audit_annotations operations =
  outcome ?warnings ?audit_annotations (Patched operations)

let ( let* ) result fn =
  match result with
  | Ok value -> fn value
  | Error _ as error -> error

let fields context = function
  | `Assoc fields ->
      let seen = Hashtbl.create (List.length fields) in
      let rec validate = function
        | [] -> Ok fields
        | (name, _) :: rest ->
            if Hashtbl.mem seen name then
              Error (context ^ ": duplicate field " ^ name)
            else (
              Hashtbl.add seen name ();
              validate rest)
      in
      validate fields
  | _ -> Error (context ^ " must be an object")

let required context name fields decode =
  match List.assoc_opt name fields with
  | None -> Error (context ^ "." ^ name ^ " is required")
  | Some value -> decode (context ^ "." ^ name) value

let optional context name fields decode =
  match List.assoc_opt name fields with
  | None | Some `Null -> Ok None
  | Some value ->
      let* value = decode (context ^ "." ^ name) value in
      Ok (Some value)

let string context = function
  | `String value -> Ok value
  | _ -> Error (context ^ " must be a string")

let bool context = function
  | `Bool value -> Ok value
  | _ -> Error (context ^ " must be a boolean")

let string_list context = function
  | `List values ->
      let rec decode index accumulator = function
        | [] -> Ok (List.rev accumulator)
        | value :: rest ->
            let* value = string (Printf.sprintf "%s[%d]" context index) value in
            decode (index + 1) (value :: accumulator) rest
      in
      decode 0 [] values
  | _ -> Error (context ^ " must be an array")

let gvk context json =
  let* fields = fields context json in
  let* group = required context "group" fields string in
  let* version = required context "version" fields string in
  let* kind = required context "kind" fields string in
  if version = "" then Error (context ^ ".version must not be empty")
  else if kind = "" then Error (context ^ ".kind must not be empty")
  else Ok { group; version; kind }

let gvr context json =
  let* fields = fields context json in
  let* group = required context "group" fields string in
  let* version = required context "version" fields string in
  let* resource = required context "resource" fields string in
  if version = "" then Error (context ^ ".version must not be empty")
  else if resource = "" then Error (context ^ ".resource must not be empty")
  else Ok { group; version; resource }

let operation context json =
  let* value = string context json in
  Ok
    (match value with
    | "CREATE" -> Create
    | "UPDATE" -> Update
    | "DELETE" -> Delete
    | "CONNECT" -> Connect
    | value -> Unknown value)

let extra context json =
  let* fields = fields context json in
  let rec decode accumulator = function
    | [] -> Ok (List.rev accumulator)
    | (name, value) :: rest ->
        let* values = string_list (context ^ "." ^ name) value in
        decode ((name, values) :: accumulator) rest
  in
  decode [] fields

let user_info context json =
  let* fields = fields context json in
  let* username = required context "username" fields string in
  let* uid = optional context "uid" fields string in
  let* groups = optional context "groups" fields string_list in
  let* extra = optional context "extra" fields extra in
  Ok
    {
      username;
      uid;
      groups = Option.value ~default:[] groups;
      extra = Option.value ~default:[] extra;
    }

let request_of_json context json =
  let* fields = fields context json in
  let* uid = required context "uid" fields string in
  let* kind = required context "kind" fields gvk in
  let* resource = required context "resource" fields gvr in
  let* subresource = optional context "subResource" fields string in
  let* request_kind = optional context "requestKind" fields gvk in
  let* request_resource = optional context "requestResource" fields gvr in
  let* request_subresource =
    optional context "requestSubResource" fields string
  in
  let* name = optional context "name" fields string in
  let* namespace = optional context "namespace" fields string in
  let* operation = required context "operation" fields operation in
  let* user_info = required context "userInfo" fields user_info in
  let* object_ = optional context "object" fields (fun _ value -> Ok value) in
  let* old_object =
    optional context "oldObject" fields (fun _ value -> Ok value)
  in
  let* dry_run = optional context "dryRun" fields bool in
  let* options = optional context "options" fields (fun _ value -> Ok value) in
  if uid = "" then Error (context ^ ".uid must not be empty")
  else
    Ok
      {
        uid;
        kind;
        resource;
        subresource;
        request_kind;
        request_resource;
        request_subresource;
        name;
        namespace;
        operation;
        user_info;
        object_;
        old_object;
        dry_run;
        options;
      }

let request_of_review_json json =
  let* review = fields "AdmissionReview" json in
  let* api_version = required "AdmissionReview" "apiVersion" review string in
  let* kind = required "AdmissionReview" "kind" review string in
  if api_version <> "admission.k8s.io/v1" then
    Error ("unsupported AdmissionReview apiVersion " ^ api_version)
  else if kind <> "AdmissionReview" then
    Error ("unexpected AdmissionReview kind " ^ kind)
  else required "AdmissionReview" "request" review request_of_json

let gvk_to_json (value : group_version_kind) =
  `Assoc
    [
      ("group", `String value.group);
      ("version", `String value.version);
      ("kind", `String value.kind);
    ]

let gvr_to_json (value : group_version_resource) =
  `Assoc
    [
      ("group", `String value.group);
      ("version", `String value.version);
      ("resource", `String value.resource);
    ]

let operation_to_string = function
  | Create -> "CREATE"
  | Update -> "UPDATE"
  | Delete -> "DELETE"
  | Connect -> "CONNECT"
  | Unknown value -> value

let user_info_to_json value =
  let optional name encode = function
    | None -> []
    | Some value -> [ (name, encode value) ]
  in
  `Assoc
    ([ ("username", `String value.username) ]
    @ optional "uid" (fun value -> `String value) value.uid
    @
    if value.groups = [] then []
    else
      [ ("groups", `List (List.map (fun value -> `String value) value.groups)) ]
      @
      if value.extra = [] then []
      else
        [
          ( "extra",
            `Assoc
              (List.map
                 (fun (name, values) ->
                   (name, `List (List.map (fun value -> `String value) values)))
                 value.extra) );
        ])

let request_to_json value =
  let optional name encode = function
    | None -> []
    | Some value -> [ (name, encode value) ]
  in
  `Assoc
    ([
       ("uid", `String value.uid);
       ("kind", gvk_to_json value.kind);
       ("resource", gvr_to_json value.resource);
     ]
    @ optional "subResource" (fun value -> `String value) value.subresource
    @ optional "requestKind" gvk_to_json value.request_kind
    @ optional "requestResource" gvr_to_json value.request_resource
    @ optional "requestSubResource"
        (fun value -> `String value)
        value.request_subresource
    @ optional "name" (fun value -> `String value) value.name
    @ optional "namespace" (fun value -> `String value) value.namespace
    @ [
        ("operation", `String (operation_to_string value.operation));
        ("userInfo", user_info_to_json value.user_info);
      ]
    @ optional "object" Fun.id value.object_
    @ optional "oldObject" Fun.id value.old_object
    @ optional "dryRun" (fun value -> `Bool value) value.dry_run
    @ optional "options" Fun.id value.options)

let request_to_review_json request =
  `Assoc
    [
      ("apiVersion", `String "admission.k8s.io/v1");
      ("kind", `String "AdmissionReview");
      ("request", request_to_json request);
    ]

let patch_operation_to_json = function
  | Add { path; value } ->
      `Assoc [ ("op", `String "add"); ("path", `String path); ("value", value) ]
  | Remove { path } ->
      `Assoc [ ("op", `String "remove"); ("path", `String path) ]
  | Replace { path; value } ->
      `Assoc
        [ ("op", `String "replace"); ("path", `String path); ("value", value) ]
  | Move { from; path } ->
      `Assoc
        [
          ("op", `String "move"); ("from", `String from); ("path", `String path);
        ]
  | Copy { from; path } ->
      `Assoc
        [
          ("op", `String "copy"); ("from", `String from); ("path", `String path);
        ]
  | Test { path; value } ->
      `Assoc
        [ ("op", `String "test"); ("path", `String path); ("value", value) ]

let status_json denial =
  `Assoc
    [
      ("apiVersion", `String "v1");
      ("kind", `String "Status");
      ("status", `String "Failure");
      ("message", `String denial.message);
      ("reason", `String denial.reason);
      ("code", `Int denial.code);
    ]

let response_to_review_json uid outcome =
  let allowed, status, patch =
    match outcome.decision with
    | Allowed -> (true, [], [])
    | Denied denial -> (false, [ ("status", status_json denial) ], [])
    | Patched operations ->
        let encoded =
          operations |> List.map patch_operation_to_json |> fun values ->
          `List values |> Yojson.Safe.to_string |> Base64.encode_string
        in
        ( true,
          [],
          [ ("patch", `String encoded); ("patchType", `String "JSONPatch") ] )
  in
  let response =
    [ ("uid", `String uid); ("allowed", `Bool allowed) ]
    @ status @ patch
    @ (if outcome.audit_annotations = [] then []
       else
         [
           ( "auditAnnotations",
             `Assoc
               (List.map
                  (fun (key, value) -> (key, `String value))
                  outcome.audit_annotations) );
         ])
    @
    if outcome.warnings = [] then []
    else
      [
        ( "warnings",
          `List (List.map (fun warning -> `String warning) outcome.warnings) );
      ]
  in
  `Assoc
    [
      ("apiVersion", `String "admission.k8s.io/v1");
      ("kind", `String "AdmissionReview");
      ("response", `Assoc response);
    ]

let internal_error message = deny ~code:500 ~reason:"InternalError" message

let respond ~cancel handler json =
  let* request = request_of_review_json json in
  let outcome =
    try
      match handler ~cancel request with
      | Ok outcome -> outcome
      | Error message -> internal_error message
    with exn ->
      internal_error
        ("uncaught admission handler exception: " ^ Printexc.to_string exn)
  in
  Ok (response_to_review_json request.uid outcome)

module For (Resource : Core.Resource) = struct
  type typed_request = {
    admission : request;
    object_ : Resource.t option;
    old_object : Resource.t option;
  }

  let decode field = function
    | None -> Ok None
    | Some json -> (
        match Resource.of_json json with
        | Ok value -> Ok (Some value)
        | Error message -> Error (field ^ ": " ^ message))

  let expected_gvk : group_version_kind =
    {
      group = Resource.api.group;
      version = Resource.api.version;
      kind = Resource.api.kind;
    }

  let handler callback ~cancel admission =
    if admission.kind <> expected_gvk then
      Ok
        (deny ~code:400 ~reason:"BadRequest"
           (Printf.sprintf "expected %s, received %s/%s, Kind=%s"
              (Core.api_version Resource.api)
              admission.kind.group admission.kind.version admission.kind.kind))
    else
      match decode "object" admission.object_ with
      | Error message -> Ok (deny ~code:400 ~reason:"BadRequest" message)
      | Ok object_ -> (
          match decode "oldObject" admission.old_object with
          | Error message -> Ok (deny ~code:400 ~reason:"BadRequest" message)
          | Ok old_object -> callback ~cancel { admission; object_; old_object }
          )
end
