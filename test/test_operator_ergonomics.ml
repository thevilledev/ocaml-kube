module K = Kube
module C = Kube_crd
module Test = Kube_test.Transport

let widget_api =
  {
    K.Core.group = "testing.ocaml-kube.dev";
    version = "v1";
    kind = "Widget";
    plural = "widgets";
    scope = Namespaced;
  }

module Widget = struct
  type t = K.Dynamic.t

  let api = widget_api
  let metadata value = value.K.Dynamic.metadata
  let of_json = K.Dynamic.of_json
  let to_json = K.Dynamic.to_json
end

module Widget_reconcile = K.Reconcile.Make (Widget)

let metadata ?uid ?(namespace = "operators") name =
  {
    K.Core.name;
    namespace = Some namespace;
    uid;
    resource_version = None;
    generation = None;
    deletion_timestamp = None;
    finalizers = [];
    owner_references = [];
    labels = [];
    annotations = [];
  }

let widget ?uid ?(namespace = "operators") name =
  K.Dynamic.of_json
    (`Assoc
       [
         ("apiVersion", `String "testing.ocaml-kube.dev/v1");
         ("kind", `String "Widget");
         ("metadata", K.Core.object_meta_to_json (metadata ?uid ~namespace name));
         ("spec", `Assoc [ ("enabled", `Bool true) ]);
       ])
  |> Result.get_ok

let test_apply_owned () =
  let owner = metadata ~uid:"owner-uid" "parent" in
  let desired = widget "child" in
  let response = Widget.to_json desired in
  let transport = Test.scripted [ Test.respond_json response ] in
  let client = Kube_test.client transport in
  let result =
    Widget_reconcile.apply_owned client ~field_manager:"widget-controller"
      ~owner_api:widget_api ~owner desired
  in
  (match result with
  | Ok _ -> ()
  | Error error -> Alcotest.failf "%a" K.Client.pp_error error);
  let request = List.hd (Test.requests transport) in
  let target = Uri.of_string request.target in
  Alcotest.(check string)
    "metadata namespace used"
    "/apis/testing.ocaml-kube.dev/v1/namespaces/operators/widgets/child"
    (Uri.path target);
  Alcotest.(check (option string))
    "field manager" (Some "widget-controller")
    (Uri.get_query_param target "fieldManager");
  let body = Option.get request.body |> Yojson.Safe.from_string in
  let owner_references =
    match body with
    | `Assoc root -> (
        match List.assoc "metadata" root with
        | `Assoc metadata -> (
            match List.assoc "ownerReferences" metadata with
            | `List values -> values
            | _ -> Alcotest.fail "ownerReferences was not an array")
        | _ -> Alcotest.fail "metadata was not an object")
    | _ -> Alcotest.fail "apply body was not an object"
  in
  Alcotest.(check int) "one owner" 1 (List.length owner_references);
  Alcotest.(check bool)
    "controller reference" true
    (match List.hd owner_references with
    | `Assoc fields -> List.assoc_opt "controller" fields = Some (`Bool true)
    | _ -> false);
  Test.verify_complete transport |> Result.get_ok

let test_owner_guards () =
  let owner = metadata ~uid:"owner-uid" "parent" in
  let cross_namespace = widget ~namespace:"other" "child" in
  Alcotest.(check bool)
    "cross namespace rejected" true
    (Result.is_error
       (Widget_reconcile.set_controller_reference ~owner_api:widget_api ~owner
          cross_namespace));
  let existing =
    K.Core.controller_owner_reference widget_api
      (metadata ~uid:"other-uid" "other-parent")
    |> Result.get_ok
  in
  let child = widget "child" in
  let json = Widget.to_json child in
  let json =
    match json with
    | `Assoc root ->
        let metadata =
          match List.assoc "metadata" root with
          | `Assoc fields ->
              `Assoc
                (( "ownerReferences",
                   `List [ K.Core.owner_reference_to_json existing ] )
                :: List.remove_assoc "ownerReferences" fields)
          | _ -> assert false
        in
        `Assoc (("metadata", metadata) :: List.remove_assoc "metadata" root)
    | _ -> assert false
  in
  let child = Widget.of_json json |> Result.get_ok in
  Alcotest.(check bool)
    "controller takeover rejected" true
    (Result.is_error
       (Widget_reconcile.set_controller_reference ~owner_api:widget_api ~owner
          child));
  let wrong_type_meta =
    let value =
      match child.value with
      | `Assoc fields ->
          `Assoc
            (("apiVersion", `String "testing.ocaml-kube.dev/v2")
            :: List.remove_assoc "apiVersion" fields)
      | _ -> assert false
    in
    { child with value }
  in
  let transport = Test.create (fun _ -> Test.fail "unexpected request") in
  let client = Kube_test.client transport in
  Alcotest.(check bool)
    "conflicting type metadata rejected" true
    (Result.is_error
       (Widget_reconcile.apply client ~field_manager:"widget-controller"
          wrong_type_meta));
  Alcotest.(check int) "invalid apply stays local" 0
    (Test.request_count transport)

let api_error code reason =
  K.Client.Api
    {
      code;
      reason = Some reason;
      message = reason;
      retry_after_seconds = None;
      body = None;
    }

let test_retry_on_conflict () =
  let attempts = ref 0 in
  let backoff =
    K.Retry.exponential ~initial:0. ~maximum:0. ~factor:1. ~jitter:0.
      ~max_attempts:3 ()
  in
  let result =
    K.Retry.on_conflict ~backoff (fun () ->
        incr attempts;
        if !attempts < 3 then Error (api_error 409 "Conflict") else Ok "done")
  in
  Alcotest.(check (result string string))
    "eventual result" (Ok "done")
    (Result.map_error (Format.asprintf "%a" K.Client.pp_error) result);
  Alcotest.(check int) "fresh callback each time" 3 !attempts;
  attempts := 0;
  let result =
    K.Retry.on_conflict ~backoff (fun () ->
        incr attempts;
        Error (api_error 422 "Invalid"))
  in
  Alcotest.(check bool) "non-conflict returned" true (Result.is_error result);
  Alcotest.(check int) "non-conflict not retried" 1 !attempts

let test_conditions () =
  let ready =
    C.Condition.make ~last_transition_time:"2026-09-04T10:00:00Z"
      ~observed_generation:1L ~type_:"Ready" ~status:C.Condition.False
      ~reason:"Starting" ~message:"Waiting for a child" ()
  in
  let conditions, changed = C.Condition.set ready [] in
  Alcotest.(check bool) "insert changed" true changed;
  let same_status = { ready with reason = "StillStarting" } in
  let conditions, changed =
    C.Condition.set ~now:(fun () -> "never") same_status conditions
  in
  Alcotest.(check bool) "reason changed" true changed;
  Alcotest.(check string)
    "same status keeps transition" "2026-09-04T10:00:00Z"
    (List.hd conditions).last_transition_time;
  let ready =
    { same_status with status = C.Condition.True; reason = "Available" }
  in
  let conditions, _ =
    C.Condition.set ~now:(fun () -> "2026-09-04T10:01:00Z") ready conditions
  in
  Alcotest.(check bool) "ready" true (C.Condition.is_true "Ready" conditions);
  Alcotest.(check string)
    "status change advances transition" "2026-09-04T10:01:00Z"
    (List.hd conditions).last_transition_time;
  let encoded = C.Condition.to_json (List.hd conditions) in
  Alcotest.(check bool)
    "condition round trip" true
    (match C.Condition.of_json encoded with
    | Ok decoded -> decoded = List.hd conditions
    | Error _ -> false);
  Alcotest.(check bool)
    "condition schema valid" true
    (Result.is_ok (C.Schema.validate C.Condition.schema))

let test_operator_runner () =
  let transport = Test.create (fun _ -> Test.fail "unexpected request") in
  let client = Kube_test.client transport in
  let cancel = K.Cancel.create () in
  K.Cancel.cancel cancel;
  let options =
    K.Operator.Options.make ~namespace:"operators" ~workers:3
      ~identity:"operator-test" ~leader_election_name:"widget-operator" ()
  in
  let built = ref false in
  let result =
    K.Operator.run_with_client ~cancel options client
      ~components:(fun context ->
        built := true;
        Alcotest.(check (option string))
          "namespace context" (Some "operators") context.namespace;
        Alcotest.(check int) "worker context" 3 context.workers;
        Alcotest.(check string)
          "identity context" "operator-test" context.identity;
        [ K.Manager.component ~name:"proof" (fun ~client:_ ~cancel:_ -> Ok ()) ])
  in
  Alcotest.(check bool) "components built" true !built;
  Alcotest.(check (result unit string)) "runner" (Ok ()) result;
  Alcotest.(check int) "no API traffic" 0 (Test.request_count transport);
  let invalid = { options with workers = 0 } in
  Alcotest.(check (result unit string))
    "invalid options" (Error "--workers must be positive")
    (K.Operator.run_with_client invalid client ~components:(fun _ -> []))

let signal_test_kubeconfig =
  {|{
  "apiVersion": "v1",
  "kind": "Config",
  "clusters": [
    {"name": "local", "cluster": {"server": "http://127.0.0.1:1"}}
  ],
  "contexts": [
    {"name": "local", "context": {"cluster": "local", "user": "local"}}
  ],
  "current-context": "local",
  "users": [{"name": "local", "user": {"token": "test"}}]
}|}

let test_operator_signal_shutdown () =
  let ready_read, ready_write = Unix.pipe ~cloexec:true () in
  match Unix.fork () with
  | 0 ->
      Unix.close ready_read;
      let kubeconfig = Filename.temp_file "kube-operator-signal-" ".json" in
      let output = open_out_bin kubeconfig in
      output_string output signal_test_kubeconfig;
      close_out output;
      let options =
        K.Operator.Options.make ~kubeconfig
          ~leader_election_name:"signal-test" ()
      in
      let result =
        K.Operator.run
          ~logger:(K.Log.create ~sink:(fun _ -> ()) ())
          options ~components:(fun _ ->
            [
              K.Manager.component ~name:"waiting" (fun ~client:_ ~cancel ->
                  ignore (Unix.write_substring ready_write "r" 0 1);
                  while K.Cancel.sleep cancel 60. do
                    ()
                  done;
                  Ok ());
            ])
      in
      Unix.close ready_write;
      Sys.remove kubeconfig;
      Unix._exit (if Result.is_ok result then 0 else 1)
  | child ->
      Unix.close ready_write;
      let reaped = ref false in
      Fun.protect
        ~finally:(fun () ->
          Unix.close ready_read;
          if not !reaped then (
            (try Unix.kill child Sys.sigkill with Unix.Unix_error _ -> ());
            try ignore (Unix.waitpid [] child) with Unix.Unix_error _ -> ()))
        (fun () ->
          let readable, _, _ = Unix.select [ ready_read ] [] [] 5. in
          if readable = [] then
            Alcotest.fail "operator component did not start";
          let byte = Bytes.create 1 in
          Alcotest.(check int)
            "readiness byte" 1 (Unix.read ready_read byte 0 1);
          Unix.kill child Sys.sigterm;
          let deadline = Unix.gettimeofday () +. 5. in
          let rec await () =
            match Unix.waitpid [ Unix.WNOHANG ] child with
            | 0, _ when Unix.gettimeofday () < deadline ->
                Thread.delay 0.01;
                await ()
            | 0, _ -> Alcotest.fail "operator did not stop after SIGTERM"
            | _, status ->
                reaped := true;
                status
          in
          match await () with
          | Unix.WEXITED 0 -> ()
          | Unix.WEXITED code ->
              Alcotest.failf "operator exited with status %d" code
          | Unix.WSIGNALED signal | Unix.WSTOPPED signal ->
              Alcotest.failf "operator ended on signal %d" signal)

let () =
  Alcotest.run "Operator ergonomics"
    [
      ( "reconcile",
        [
          Alcotest.test_case "apply owned child" `Quick test_apply_owned;
          Alcotest.test_case "ownership guards" `Quick test_owner_guards;
          Alcotest.test_case "retry conflict" `Quick test_retry_on_conflict;
        ] );
      ( "status",
        [ Alcotest.test_case "standard conditions" `Quick test_conditions ] );
      ( "process",
        [
          Alcotest.test_case "operator runner" `Quick test_operator_runner;
          Alcotest.test_case "SIGTERM shutdown" `Quick
            test_operator_signal_shutdown;
        ] );
    ]
