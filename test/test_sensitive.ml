module K = Kube

let config ?(credential = K.Config.Anonymous) () =
  K.Config.make ~credential (Uri.of_string "https://kubernetes.example.test")

let secret_response ?(status = 200) body =
  {
    K.Http.Sensitive.status;
    reason = (if status = 200 then "OK" else "error");
    headers = [];
    body = Secret.of_string body;
  }

let ordinary _ = Error "ordinary transport must not be used"

let contains haystack needle =
  let haystack_length = String.length haystack in
  let needle_length = String.length needle in
  let rec equal_at start index =
    if index = needle_length then true
    else if haystack.[start + index] <> needle.[index] then false
    else equal_at start (index + 1)
  in
  let rec search start =
    if start + needle_length > haystack_length then false
    else if equal_at start 0 then true
    else search (start + 1)
  in
  search 0

let body_contains request needle =
  match request.K.Client.Transport.body with
  | None -> false
  | Some body ->
      Secret.Unsafe.with_string_view body (fun view -> contains view needle)

let test_strict_rejects_heap_credential () =
  let called = ref false in
  let transport =
    K.Client.Transport.make
      ~sensitive:(fun _ ->
        called := true;
        Ok (secret_response "{}"))
      ordinary
  in
  let client =
    K.Client.create_with_transport ~transport
      (config ~credential:(K.Config.Static_token "heap-token") ())
  in
  (match K.Client.Sensitive.raw client `GET "/api" with
  | Error (K.Client.Invalid_request _) -> ()
  | Error error -> Alcotest.failf "unexpected error: %a" K.Client.pp_error error
  | Ok response ->
      Secret.destroy response.body;
      Alcotest.fail "heap credential was accepted in strict mode");
  Alcotest.(check bool) "transport not called" false !called;
  K.Client.close client

let test_token_file_is_scoped () =
  let path = Filename.temp_file "kube-sensitive" ".token" in
  let channel = open_out_bin path in
  output_string channel "token-from-file\n";
  close_out channel;
  let borrowed = ref None in
  let saw_authorization = ref false in
  let transport =
    K.Client.Transport.make
      ~sensitive:(fun request ->
        List.iter
          (fun (name, value) ->
            if name = "Authorization" then (
              borrowed := Some value;
              saw_authorization :=
                Secret.equal_string value "Bearer token-from-file"))
          request.K.Client.Transport.secret_headers;
        Ok (secret_response "{}"))
      ordinary
  in
  let client =
    K.Client.create_with_transport ~transport
      (config ~credential:(K.Config.Token_file path) ())
  in
  let response =
    match K.Client.Sensitive.raw client `GET "/api" with
    | Ok response -> response
    | Error error -> Alcotest.failf "request failed: %a" K.Client.pp_error error
  in
  Secret.destroy response.body;
  Alcotest.(check bool) "authorization value" true !saw_authorization;
  Alcotest.(check bool)
    "borrow ended" true
    (Option.fold ~none:false ~some:Secret.is_destroyed !borrowed);
  K.Client.close client;
  Sys.remove path

let test_create_and_patch_secret () =
  let create_seen = ref false in
  let patch_seen = ref false in
  let response_bodies = ref [] in
  let transport =
    K.Client.Transport.make
      ~sensitive:(fun request ->
        (match request.K.Client.Transport.meth with
        | `POST ->
            create_seen :=
              request.target = "/api/v1/namespaces/demo/secrets"
              && body_contains request "\"password\":\"czNjcjN0\""
        | `PATCH ->
            patch_seen :=
              request.target = "/api/v1/namespaces/demo/secrets/target"
              && body_contains request
                   "\"path\":\"/metadata/annotations/ocaml.example~1source\""
              && body_contains request "\"password\":\"czNjcjN0\""
        | _ -> ());
        let response = secret_response "{}" in
        response_bodies := response.body :: !response_bodies;
        Ok response)
      ordinary
  in
  let client = K.Client.create_with_transport ~transport (config ()) in
  let value = Secret.of_string "s3cr3t" in
  Fun.protect
    ~finally:(fun () -> Secret.destroy value)
    (fun () ->
      let manifest : K.Client.Sensitive.secret_manifest =
        {
          namespace = "demo";
          name = "target";
          type_ = Some "Opaque";
          immutable = None;
          labels = [];
          annotations = [ ("ocaml.example/source", "uid-1") ];
          owner_references = [];
          data = [ ("password", value) ];
        }
      in
      (match K.Client.Sensitive.create_secret client manifest with
      | Ok _ -> ()
      | Error error ->
          Alcotest.failf "Secret create failed: %a" K.Client.pp_error error);
      match
        K.Client.Sensitive.patch_secret client ~namespace:"demo" ~name:"target"
          ~source_annotation:("ocaml.example/source", "uid-1")
          ~data:[ ("password", value) ]
      with
      | Ok _ -> ()
      | Error error ->
          Alcotest.failf "Secret patch failed: %a" K.Client.pp_error error);
  Alcotest.(check bool) "create wire body" true !create_seen;
  Alcotest.(check bool) "patch wire body" true !patch_seen;
  Alcotest.(check bool)
    "response buffers destroyed" true
    (List.for_all Secret.is_destroyed !response_bodies);
  K.Client.close client

let test_token_request () =
  let response_body = ref None in
  let saw_request = ref false in
  let transport =
    K.Client.Transport.make
      ~sensitive:(fun request ->
        saw_request :=
          request.K.Client.Transport.target
          = "/api/v1/namespaces/demo/serviceaccounts/vault-auth/token"
          && body_contains request "\"audiences\":[\"vault\"]";
        let response =
          secret_response
            "{\"apiVersion\":\"authentication.k8s.io/v1\",\"status\":{\"token\":\"jwt-value\"}}"
        in
        response_body := Some response.body;
        Ok response)
      ordinary
  in
  let client = K.Client.create_with_transport ~transport (config ()) in
  let token =
    match
      K.Client.Sensitive.create_service_account_token client ~namespace:"demo"
        ~service_account:"vault-auth" ~audiences:[ "vault" ]
    with
    | Ok token -> token
    | Error error ->
        Alcotest.failf "TokenRequest failed: %a" K.Client.pp_error error
  in
  Alcotest.(check bool) "request" true !saw_request;
  Alcotest.(check bool) "token" true (Secret.equal_string token "jwt-value");
  Alcotest.(check bool)
    "response destroyed" true
    (Option.fold ~none:false ~some:Secret.is_destroyed !response_body);
  Secret.destroy token;
  K.Client.close client

let test_sensitive_error_is_sanitized () =
  let response_body = ref None in
  let transport =
    K.Client.Transport.make
      ~sensitive:(fun _ ->
        let response = secret_response ~status:409 "sensitive-server-body" in
        response_body := Some response.body;
        Ok response)
      ordinary
  in
  let client = K.Client.create_with_transport ~transport (config ()) in
  let value = Secret.of_string "value" in
  Fun.protect
    ~finally:(fun () -> Secret.destroy value)
    (fun () ->
      let manifest : K.Client.Sensitive.secret_manifest =
        {
          namespace = "demo";
          name = "collision";
          type_ = None;
          immutable = None;
          labels = [];
          annotations = [];
          owner_references = [];
          data = [ ("key", value) ];
        }
      in
      match K.Client.Sensitive.create_secret client manifest with
      | Error error ->
          Alcotest.(check bool)
            "classified without response JSON" true
            (K.Client.Error.is_already_exists error);
          let rendered = Format.asprintf "%a" K.Client.pp_error error in
          Alcotest.(check bool)
            "body absent" false
            (contains rendered "sensitive-server-body")
      | Ok _ -> Alcotest.fail "409 create succeeded");
  Alcotest.(check bool)
    "error response destroyed" true
    (Option.fold ~none:false ~some:Secret.is_destroyed !response_body);
  K.Client.close client

let () =
  Alcotest.run "sensitive Kubernetes transport"
    [
      ( "client",
        [
          Alcotest.test_case "strict credential origin" `Quick
            test_strict_rejects_heap_credential;
          Alcotest.test_case "token-file scope" `Quick test_token_file_is_scoped;
          Alcotest.test_case "Secret create and patch" `Quick
            test_create_and_patch_secret;
          Alcotest.test_case "TokenRequest" `Quick test_token_request;
          Alcotest.test_case "sanitized API error" `Quick
            test_sensitive_error_is_sanitized;
        ] );
    ]
