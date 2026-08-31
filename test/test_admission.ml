module K = Kube
module A = K.Admission
module C = K.Conversion
module J = Yojson.Safe.Util

let require_ok context = function
  | Ok value -> value
  | Error message -> Alcotest.failf "%s: %s" context message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let fixture name =
  let local = Filename.concat "fixtures" name in
  if Sys.file_exists local then local else Filename.concat "test/fixtures" name

let certificate_pem () = read (fixture "webhook-cert.pem")
let private_key_pem () = read (fixture "webhook-key.pem")

let widget_kind : A.group_version_kind =
  { group = "demo.ocaml-kube.dev"; version = "v1"; kind = "Widget" }

let widget_resource : A.group_version_resource =
  { group = "demo.ocaml-kube.dev"; version = "v1"; resource = "widgets" }

let widget_json name =
  `Assoc
    [
      ("apiVersion", `String "demo.ocaml-kube.dev/v1");
      ("kind", `String "Widget");
      ( "metadata",
        `Assoc [ ("name", `String name); ("namespace", `String "default") ] );
      ("spec", `Assoc [ ("message", `String "hello") ]);
    ]

let request ?(kind = widget_kind) ?(object_ = Some (widget_json "sample"))
    ?(old_object = None) () : A.request =
  {
    uid = "request-uid";
    kind;
    resource = widget_resource;
    subresource = None;
    request_kind = Some kind;
    request_resource = Some widget_resource;
    request_subresource = None;
    name = Some "sample";
    namespace = Some "default";
    operation = Create;
    user_info =
      {
        username = "system:serviceaccount:kube-system:apiserver";
        uid = Some "user-uid";
        groups = [ "system:serviceaccounts"; "system:authenticated" ];
        extra = [ ("authentication.kubernetes.io/pod-name", [ "apiserver" ]) ];
      };
    object_;
    old_object;
    dry_run = Some false;
    options = Some (`Assoc [ ("apiVersion", `String "meta.k8s.io/v1") ]);
  }

let response review = review |> J.member "response"
let allowed review = review |> response |> J.member "allowed" |> J.to_bool

let status_code review =
  review |> response |> J.member "status" |> J.member "code" |> J.to_int

let test_review_round_trip () =
  let value = request ~old_object:(Some (widget_json "previous")) () in
  let encoded = A.request_to_review_json value in
  let decoded =
    A.request_of_review_json encoded |> require_ok "decode review"
  in
  Alcotest.(check bool) "complete request round trip" true (decoded = value);
  let unknown =
    { value with operation = A.Unknown "FUTURE"; request_kind = None }
  in
  let decoded =
    A.request_to_review_json unknown
    |> A.request_of_review_json
    |> require_ok "decode future operation"
  in
  Alcotest.(check bool) "unknown operation preserved" true (decoded = unknown)

let test_patch_response () =
  let operations =
    [
      A.Add { path = "/metadata/labels"; value = `Assoc [] };
      A.Remove { path = "/metadata/annotations/old" };
      A.Replace { path = "/spec/message"; value = `String "updated" };
      A.Move { from = "/spec/source"; path = "/spec/target" };
      A.Copy { from = "/metadata/name"; path = "/metadata/labels/name" };
      A.Test { path = "/kind"; value = `String "Widget" };
    ]
  in
  let handler ~cancel:_ _ =
    Ok
      (A.patch
         ~warnings:[ "defaulted spec.message" ]
         ~audit_annotations:[ ("demo.ocaml-kube.dev/mutated", "true") ]
         operations)
  in
  let review =
    A.respond ~cancel:(K.Cancel.create ()) handler
      (A.request_to_review_json (request ()))
    |> require_ok "mutating response"
  in
  Alcotest.(check bool) "mutation allowed" true (allowed review);
  Alcotest.(check string)
    "response UID" "request-uid"
    (review |> response |> J.member "uid" |> J.to_string);
  Alcotest.(check string)
    "patch type" "JSONPatch"
    (review |> response |> J.member "patchType" |> J.to_string);
  let patch =
    review |> response |> J.member "patch" |> J.to_string |> Base64.decode_exn
    |> Yojson.Safe.from_string
  in
  let expected =
    `List
      [
        `Assoc
          [
            ("op", `String "add");
            ("path", `String "/metadata/labels");
            ("value", `Assoc []);
          ];
        `Assoc
          [
            ("op", `String "remove");
            ("path", `String "/metadata/annotations/old");
          ];
        `Assoc
          [
            ("op", `String "replace");
            ("path", `String "/spec/message");
            ("value", `String "updated");
          ];
        `Assoc
          [
            ("op", `String "move");
            ("from", `String "/spec/source");
            ("path", `String "/spec/target");
          ];
        `Assoc
          [
            ("op", `String "copy");
            ("from", `String "/metadata/name");
            ("path", `String "/metadata/labels/name");
          ];
        `Assoc
          [
            ("op", `String "test");
            ("path", `String "/kind");
            ("value", `String "Widget");
          ];
      ]
  in
  Alcotest.(check string)
    "RFC 6902 payload"
    (Yojson.Safe.to_string expected)
    (Yojson.Safe.to_string patch);
  Alcotest.(check string)
    "warning" "defaulted spec.message"
    (review |> response |> J.member "warnings" |> J.index 0 |> J.to_string);
  Alcotest.(check string)
    "audit annotation" "true"
    (review |> response
    |> J.member "auditAnnotations"
    |> J.member "demo.ocaml-kube.dev/mutated"
    |> J.to_string)

let test_protocol_failures () =
  let wrong_version =
    match A.request_to_review_json (request ()) with
    | `Assoc fields ->
        `Assoc
          (("apiVersion", `String "admission.k8s.io/v1beta1")
          :: List.remove_assoc "apiVersion" fields)
    | _ -> assert false
  in
  Alcotest.(check bool)
    "wrong review version rejected" true
    (Result.is_error (A.request_of_review_json wrong_version));
  let duplicate =
    `Assoc
      [
        ("apiVersion", `String "admission.k8s.io/v1");
        ("apiVersion", `String "admission.k8s.io/v1");
        ("kind", `String "AdmissionReview");
        ("request", `Assoc []);
      ]
  in
  Alcotest.(check bool)
    "duplicate field rejected" true
    (Result.is_error (A.request_of_review_json duplicate));
  let failing ~cancel:_ _ = Error "validator database unavailable" in
  let review =
    A.respond ~cancel:(K.Cancel.create ()) failing
      (A.request_to_review_json (request ()))
    |> require_ok "handler error response"
  in
  Alcotest.(check bool) "internal error denied" false (allowed review);
  Alcotest.(check int) "internal error status" 500 (status_code review)

module Widget = struct
  type t = { metadata : K.Core.object_meta; json : Yojson.Safe.t }

  let api =
    {
      K.Core.group = "demo.ocaml-kube.dev";
      version = "v1";
      kind = "Widget";
      plural = "widgets";
      scope = K.Core.Namespaced;
    }

  let metadata value = value.metadata

  let of_json json =
    match K.Core.object_meta_of_json json with
    | Ok metadata -> Ok { metadata; json }
    | Error message -> Error message

  let to_json value = value.json
end

module Widget_admission = A.For (Widget)

let test_typed_handler () =
  let called = ref 0 in
  let handler =
    Widget_admission.handler (fun ~cancel:_ typed ->
        incr called;
        match typed.object_ with
        | Some widget when widget.metadata.name = "sample" -> Ok (A.allow ())
        | _ -> Error "typed object missing")
  in
  let cancel = K.Cancel.create () in
  let accepted =
    A.respond ~cancel handler (A.request_to_review_json (request ()))
    |> require_ok "typed handler"
  in
  Alcotest.(check int) "typed callback called" 1 !called;
  Alcotest.(check bool) "typed object allowed" true (allowed accepted);
  let wrong_kind : A.group_version_kind = { widget_kind with kind = "Other" } in
  let rejected =
    A.respond ~cancel handler
      (A.request_to_review_json (request ~kind:wrong_kind ()))
    |> require_ok "GVK rejection"
  in
  Alcotest.(check bool) "wrong GVK denied" false (allowed rejected);
  Alcotest.(check int) "wrong GVK status" 400 (status_code rejected);
  Alcotest.(check int) "typed callback not called twice" 1 !called

let conversion_object api_version message =
  `Assoc
    [
      ("apiVersion", `String api_version);
      ("kind", `String "Widget");
      ( "metadata",
        `Assoc
          [
            ("name", `String "sample");
            ("namespace", `String "default");
            ("uid", `String "widget-uid");
          ] );
      ("spec", `Assoc [ ("message", `String message) ]);
    ]

let convert_widget ~cancel:_ ~desired_api_version = function
  | `Assoc fields ->
      Ok
        (`Assoc
           (("apiVersion", `String desired_api_version)
           :: ("spec", `Assoc [ ("message", `String "converted") ])
           :: List.remove_assoc "spec" (List.remove_assoc "apiVersion" fields)))
  | _ -> Error "widget must be an object"

let conversion_request =
  {
    C.uid = "conversion-uid";
    desired_api_version = "demo.ocaml-kube.dev/v1";
    objects = [ conversion_object "demo.ocaml-kube.dev/v1alpha1" "source" ];
  }

let conversion_status review =
  review |> response |> J.member "result" |> J.member "status" |> J.to_string

let test_conversion_review () =
  let handler = C.map convert_widget in
  let encoded = C.request_to_review_json conversion_request in
  let decoded =
    C.request_of_review_json encoded |> require_ok "decode conversion review"
  in
  Alcotest.(check bool)
    "conversion request round trip" true
    (decoded = conversion_request);
  let review =
    C.respond ~cancel:(K.Cancel.create ()) handler encoded
    |> require_ok "conversion response"
  in
  Alcotest.(check string)
    "conversion status" "Success" (conversion_status review);
  Alcotest.(check string)
    "conversion UID" "conversion-uid"
    (review |> response |> J.member "uid" |> J.to_string);
  Alcotest.(check string)
    "desired version" "demo.ocaml-kube.dev/v1"
    (review |> response
    |> J.member "convertedObjects"
    |> J.index 0 |> J.member "apiVersion" |> J.to_string);
  let wrong_count ~cancel:_ _ = Ok [] in
  let failed =
    C.respond ~cancel:(K.Cancel.create ()) wrong_count encoded
    |> require_ok "invalid conversion response"
  in
  Alcotest.(check string)
    "wrong count becomes failure" "Failure" (conversion_status failed);
  let changed_identity ~cancel:_ _ =
    Ok
      [
        ( conversion_object "demo.ocaml-kube.dev/v1" "converted" |> function
          | `Assoc fields ->
              `Assoc
                (("metadata", `Assoc [ ("name", `String "different") ])
                :: List.remove_assoc "metadata" fields)
          | _ -> assert false );
      ]
  in
  let failed =
    C.respond ~cancel:(K.Cancel.create ()) changed_identity encoded
    |> require_ok "identity conversion response"
  in
  Alcotest.(check string)
    "identity change becomes failure" "Failure" (conversion_status failed)

let dummy_config = K.Config.make (Uri.of_string "http://127.0.0.1:1")

let client_config port certificate =
  K.Config.make
    ~tls:{ K.Config.default_tls with ca_pem = Some certificate }
    (Uri.of_string (Printf.sprintf "https://127.0.0.1:%d" port))

let with_server ?(request_timeout = 1.) ?(max_body_bytes = 1024)
    ?(require_client_certificate = false) ?conversion_handler ?logger handler fn
    =
  let certificate = certificate_pem () in
  let metrics = K.Metrics.create () in
  let server =
    K.Webhook.create ~port:0 ~request_timeout ~max_body_bytes ~metrics
      ?client_ca_pem:
        (if require_client_certificate then Some certificate else None)
      ~certificate_pem:certificate ~private_key_pem:(private_key_pem ()) ()
    |> require_ok "create webhook server"
  in
  K.Webhook.add_admission server ~path:"/validate" handler;
  Option.iter
    (fun handler -> K.Webhook.add_conversion server ~path:"/convert" handler)
    conversion_handler;
  let cancel = K.Cancel.create () in
  let client =
    K.Client.create ?logger ~rate_limiter:K.Rate_limiter.unlimited dummy_config
  in
  let manager = K.Manager.create ~cancel client in
  K.Manager.add manager (K.Webhook.component server);
  let manager_result = ref None in
  let thread =
    Thread.create (fun () -> manager_result := Some (K.Manager.run manager)) ()
  in
  Fun.protect
    ~finally:(fun () ->
      K.Cancel.cancel cancel;
      Thread.join thread;
      K.Client.close client;
      match !manager_result with
      | Some (Ok ()) -> ()
      | Some (Error error) ->
          Alcotest.failf "webhook manager: %a" K.Manager.pp_error error
      | None -> Alcotest.fail "webhook manager returned no result")
    (fun () ->
      let port =
        K.Webhook.await_listening ~cancel server
        |> require_ok "await webhook listener"
      in
      fn server metrics certificate port)

let https_request ?(headers = []) ?body config meth path =
  K.Http.request_once ~headers ?body config meth path

let test_tls_server () =
  let handler ~cancel:_ _ = Ok (A.allow ~warnings:[ "checked" ] ()) in
  let server = ref None in
  let log_events = ref [] in
  let logger =
    K.Log.create ~sink:(fun event -> log_events := event :: !log_events) ()
  in
  with_server ~conversion_handler:(C.map convert_widget) ~logger handler
    (fun running metrics certificate port ->
      server := Some running;
      let late_registration_rejected =
        try
          K.Webhook.add_admission running ~path:"/late" handler;
          false
        with Invalid_argument _ -> true
      in
      Alcotest.(check bool)
        "late registration rejected" true late_registration_rejected;
      require_ok "webhook readiness" (K.Webhook.readiness_check running ());
      let config = client_config port certificate in
      let body =
        A.request_to_review_json (request ()) |> Yojson.Safe.to_string
      in
      let valid =
        https_request
          ~headers:[ ("Content-Type", "application/json") ]
          ~body config `POST "/validate"
        |> require_ok "valid webhook request"
      in
      Alcotest.(check int) "valid HTTP status" 200 valid.status;
      Alcotest.(check bool)
        "valid review allowed" true
        (valid.body |> Yojson.Safe.from_string |> allowed);
      let wrong_method =
        https_request config `GET "/validate"
        |> require_ok "wrong method response"
      in
      Alcotest.(check int) "method status" 405 wrong_method.status;
      let missing =
        https_request
          ~headers:[ ("Content-Type", "application/json") ]
          ~body config `POST "/missing"
        |> require_ok "missing route response"
      in
      Alcotest.(check int) "missing route status" 404 missing.status;
      let media =
        https_request
          ~headers:[ ("Content-Type", "text/plain") ]
          ~body config `POST "/validate"
        |> require_ok "media response"
      in
      Alcotest.(check int) "media status" 415 media.status;
      let malformed =
        https_request
          ~headers:[ ("Content-Type", "application/json") ]
          ~body:"{" config `POST "/validate"
        |> require_ok "malformed JSON response"
      in
      Alcotest.(check int) "malformed JSON status" 400 malformed.status;
      let oversized =
        https_request
          ~headers:[ ("Content-Type", "application/json") ]
          ~body:(String.make 2048 'x') config `POST "/validate"
        |> require_ok "oversized response"
      in
      Alcotest.(check int) "oversized status" 413 oversized.status;
      let conversion_body =
        C.request_to_review_json conversion_request |> Yojson.Safe.to_string
      in
      let conversion =
        https_request
          ~headers:[ ("Content-Type", "application/json") ]
          ~body:conversion_body config `POST "/convert"
        |> require_ok "conversion webhook request"
      in
      Alcotest.(check int) "conversion HTTP status" 200 conversion.status;
      Alcotest.(check string)
        "conversion HTTPS result" "Success"
        (conversion.body |> Yojson.Safe.from_string |> conversion_status);
      let rendered = K.Metrics.render metrics in
      Alcotest.(check bool)
        "webhook metrics" true
        (String.starts_with ~prefix:"# HELP ocaml_kube_webhook_duration_seconds"
           rendered));
  (match !server with
  | None -> Alcotest.fail "webhook server was not captured"
  | Some server ->
      Alcotest.(check bool)
        "readiness fails after shutdown" true
        (Result.is_error (K.Webhook.readiness_check server ())));
  let logged message =
    List.exists
      (fun (event : K.Log.event) -> event.message = message)
      !log_events
  in
  Alcotest.(check bool)
    "webhook listening lifecycle logged" true
    (logged "Webhook server listening");
  Alcotest.(check bool)
    "webhook shutdown lifecycle logged" true
    (logged "Webhook server stopped")

let test_handler_deadline () =
  let observed = Atomic.make false in
  let handler ~cancel _ =
    let reached_deadline = not (K.Cancel.sleep cancel 10.) in
    Atomic.set observed reached_deadline;
    Ok (A.allow ())
  in
  with_server ~request_timeout:0.1 handler
    (fun _server _metrics certificate port ->
      let config = client_config port certificate in
      let body =
        A.request_to_review_json (request ()) |> Yojson.Safe.to_string
      in
      let result =
        https_request
          ~headers:[ ("Content-Type", "application/json") ]
          ~body config `POST "/validate"
      in
      Alcotest.(check bool)
        "timed-out request connection closed" true (Result.is_error result));
  Alcotest.(check bool)
    "handler observed cancellation" true (Atomic.get observed)

let test_tls_handshake_deadline () =
  let handler ~cancel:_ _ = Ok (A.allow ()) in
  with_server ~request_timeout:0.1 handler
    (fun _server _metrics _certificate port ->
      let fd = Unix.socket ~cloexec:true Unix.PF_INET Unix.SOCK_STREAM 0 in
      Fun.protect
        ~finally:(fun () -> try Unix.close fd with Unix.Unix_error _ -> ())
        (fun () ->
          Unix.connect fd (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
          let readable, _, _ = Unix.select [ fd ] [] [] 1. in
          Alcotest.(check bool)
            "stalled handshake interrupted" true (readable <> []);
          let closed =
            try
              let byte = Bytes.create 1 in
              Unix.read fd byte 0 1 = 0
            with Unix.Unix_error _ -> true
          in
          Alcotest.(check bool) "stalled socket closed" true closed))

let test_client_certificate_requirement () =
  let handler ~cancel:_ _ = Ok (A.allow ()) in
  with_server ~require_client_certificate:true handler
    (fun _server _metrics certificate port ->
      let config = client_config port certificate in
      let body =
        A.request_to_review_json (request ()) |> Yojson.Safe.to_string
      in
      let result =
        https_request
          ~headers:[ ("Content-Type", "application/json") ]
          ~body config `POST "/validate"
      in
      Alcotest.(check bool)
        "unauthenticated TLS client rejected" true (Result.is_error result))

let test_invalid_server_certificate () =
  let result =
    K.Webhook.create ~certificate_pem:"not a certificate"
      ~private_key_pem:"not a key" ()
  in
  Alcotest.(check bool) "invalid PEM rejected" true (Result.is_error result)

let () =
  Alcotest.run "Kubernetes admission webhooks"
    [
      ( "protocol",
        [
          Alcotest.test_case "review round trip" `Quick test_review_round_trip;
          Alcotest.test_case "JSON Patch response" `Quick test_patch_response;
          Alcotest.test_case "failure responses" `Quick test_protocol_failures;
          Alcotest.test_case "typed handler" `Quick test_typed_handler;
          Alcotest.test_case "conversion review" `Quick test_conversion_review;
        ] );
      ( "TLS server",
        [
          Alcotest.test_case "HTTPS routing and bounds" `Quick test_tls_server;
          Alcotest.test_case "handshake deadline" `Quick
            test_tls_handshake_deadline;
          Alcotest.test_case "handler deadline" `Quick test_handler_deadline;
          Alcotest.test_case "client certificate requirement" `Quick
            test_client_certificate_requirement;
          Alcotest.test_case "invalid certificate" `Quick
            test_invalid_server_certificate;
        ] );
    ]
