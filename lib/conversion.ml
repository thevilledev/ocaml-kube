type request = {
  uid : string;
  desired_api_version : string;
  objects : Yojson.Safe.t list;
}

type handler = cancel:Cancel.t -> request -> (Yojson.Safe.t list, string) result

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

let string context = function
  | `String value -> Ok value
  | _ -> Error (context ^ " must be a string")

let objects context = function
  | `List values -> Ok values
  | _ -> Error (context ^ " must be an array")

let request_of_json context json =
  let* fields = fields context json in
  let* uid = required context "uid" fields string in
  let* desired_api_version =
    required context "desiredAPIVersion" fields string
  in
  let* objects = required context "objects" fields objects in
  if uid = "" then Error (context ^ ".uid must not be empty")
  else if desired_api_version = "" then
    Error (context ^ ".desiredAPIVersion must not be empty")
  else Ok { uid; desired_api_version; objects }

let request_of_review_json json =
  let* review = fields "ConversionReview" json in
  let* api_version = required "ConversionReview" "apiVersion" review string in
  let* kind = required "ConversionReview" "kind" review string in
  if api_version <> "apiextensions.k8s.io/v1" then
    Error ("unsupported ConversionReview apiVersion " ^ api_version)
  else if kind <> "ConversionReview" then
    Error ("unexpected ConversionReview kind " ^ kind)
  else required "ConversionReview" "request" review request_of_json

let request_to_review_json request =
  `Assoc
    [
      ("apiVersion", `String "apiextensions.k8s.io/v1");
      ("kind", `String "ConversionReview");
      ( "request",
        `Assoc
          [
            ("uid", `String request.uid);
            ("desiredAPIVersion", `String request.desired_api_version);
            ("objects", `List request.objects);
          ] );
    ]

let map converter ~cancel request =
  let rec convert index accumulator = function
    | [] -> Ok (List.rev accumulator)
    | object_ :: rest -> (
        match
          converter ~cancel ~desired_api_version:request.desired_api_version
            object_
        with
        | Ok converted -> convert (index + 1) (converted :: accumulator) rest
        | Error message -> Error (Printf.sprintf "object %d: %s" index message))
  in
  convert 0 [] request.objects

type identity = {
  api_version : string;
  kind : string;
  name : string option;
  namespace : string option;
  uid : string option;
}

let optional_string context name fields =
  match List.assoc_opt name fields with
  | None | Some `Null -> Ok None
  | Some value ->
      let* value = string (context ^ "." ^ name) value in
      Ok (Some value)

let identity context json =
  let* object_fields = fields context json in
  let* api_version = required context "apiVersion" object_fields string in
  let* kind = required context "kind" object_fields string in
  let* metadata = required context "metadata" object_fields fields in
  let* name = optional_string (context ^ ".metadata") "name" metadata in
  let* namespace =
    optional_string (context ^ ".metadata") "namespace" metadata
  in
  let* uid = optional_string (context ^ ".metadata") "uid" metadata in
  Ok { api_version; kind; name; namespace; uid }

let validate_outputs request converted =
  if List.length converted <> List.length request.objects then
    Error
      (Printf.sprintf "converter returned %d objects for %d inputs"
         (List.length converted)
         (List.length request.objects))
  else
    let rec validate index inputs outputs =
      match (inputs, outputs) with
      | [], [] -> Ok ()
      | input :: inputs, output :: outputs ->
          let* before = identity (Printf.sprintf "objects[%d]" index) input in
          let* after =
            identity (Printf.sprintf "convertedObjects[%d]" index) output
          in
          if after.api_version <> request.desired_api_version then
            Error
              (Printf.sprintf
                 "convertedObjects[%d].apiVersion is %s, expected %s" index
                 after.api_version request.desired_api_version)
          else if after.kind <> before.kind then
            Error (Printf.sprintf "convertedObjects[%d] changed kind" index)
          else if
            (after.name, after.namespace, after.uid)
            <> (before.name, before.namespace, before.uid)
          then
            Error
              (Printf.sprintf
                 "convertedObjects[%d] changed name, namespace, or UID" index)
          else validate (index + 1) inputs outputs
      | _ -> assert false
    in
    validate 0 request.objects converted

let status_success =
  `Assoc
    [
      ("apiVersion", `String "v1");
      ("kind", `String "Status");
      ("status", `String "Success");
    ]

let status_failure message =
  `Assoc
    [
      ("apiVersion", `String "v1");
      ("kind", `String "Status");
      ("status", `String "Failure");
      ("message", `String message);
      ("reason", `String "ConversionFailed");
      ("code", `Int 500);
    ]

let response_to_review_json uid result =
  let converted_objects, result =
    match result with
    | Ok objects -> (objects, status_success)
    | Error message -> ([], status_failure message)
  in
  `Assoc
    [
      ("apiVersion", `String "apiextensions.k8s.io/v1");
      ("kind", `String "ConversionReview");
      ( "response",
        `Assoc
          [
            ("uid", `String uid);
            ("convertedObjects", `List converted_objects);
            ("result", result);
          ] );
    ]

let respond ~cancel handler json =
  let* request = request_of_review_json json in
  let converted =
    try handler ~cancel request
    with exn ->
      Error ("uncaught conversion handler exception: " ^ Printexc.to_string exn)
  in
  let converted =
    match converted with
    | Error _ as error -> error
    | Ok objects -> (
        match validate_outputs request objects with
        | Ok () -> Ok objects
        | Error _ as error -> error)
  in
  Ok (response_to_review_json request.uid converted)
