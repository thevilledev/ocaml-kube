module K = Kube

let widget_api =
  {
    K.Core.group = "testing.ocaml-k8s.dev";
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

module Widget_api = K.Client.For (Widget)
module Widget_finalizer = K.Controller.Finalizer (Widget)
module Widget_controller = K.Controller.Make (Widget)
module Widget_store = K.Store.Make (Widget)

let gadget_api =
  {
    K.Core.group = "testing.ocaml-k8s.dev";
    version = "v1";
    kind = "Gadget";
    plural = "gadgets";
    scope = Namespaced;
  }

module Gadget = struct
  type t = K.Dynamic.t

  let api = gadget_api
  let metadata value = value.K.Dynamic.metadata
  let of_json = K.Dynamic.of_json
  let to_json = K.Dynamic.to_json
end

module Widget_owns_gadget = Widget_controller.Owns (Gadget)

let test_paths () =
  let deployment =
    {
      K.Core.group = "apps";
      version = "v1";
      kind = "Deployment";
      plural = "deployments";
      scope = Namespaced;
    }
  in
  Alcotest.(check (result string string))
    "namespaced collection" (Ok "/apis/apps/v1/namespaces/team-a/deployments")
    (K.Core.collection_path deployment ~namespace:(Some "team-a"));
  Alcotest.(check (result string string))
    "all namespaces" (Ok "/apis/apps/v1/deployments")
    (K.Core.collection_path deployment ~namespace:None);
  let node =
    {
      K.Core.group = "";
      version = "v1";
      kind = "Node";
      plural = "nodes";
      scope = Cluster;
    }
  in
  Alcotest.(check (result string string))
    "cluster object" (Ok "/api/v1/nodes/worker-1")
    (K.Core.object_path node ~namespace:None ~name:"worker-1");
  Alcotest.(check bool)
    "reject namespace for cluster scope" true
    (Result.is_error (K.Core.collection_path node ~namespace:(Some "default")))

let test_object_references () =
  let metadata =
    {
      K.Core.name = "owned";
      namespace = Some "operators";
      uid = Some "uid-123";
      resource_version = Some (K.Core.Resource_version.of_string "42");
      generation = Some 3;
      deletion_timestamp = None;
      finalizers = [];
      owner_references = [];
      labels = [];
      annotations = [];
    }
  in
  let reference =
    K.Core.object_reference ~field_path:"spec.target" widget_api metadata
  in
  Alcotest.(check string)
    "reference API version" "testing.ocaml-k8s.dev/v1" reference.api_version;
  Alcotest.(check (option string))
    "reference resource version" (Some "42")
    (Option.map K.Core.Resource_version.to_string reference.resource_version);
  (match K.Core.controller_owner_reference widget_api metadata with
  | Error message -> Alcotest.fail message
  | Ok owner ->
      Alcotest.(check bool) "controller owner" true owner.controller;
      Alcotest.(check bool)
        "owner blocks deletion" true owner.block_owner_deletion;
      Alcotest.(check string) "owner UID" "uid-123" owner.uid);
  let missing_uid = { metadata with uid = None } in
  Alcotest.(check bool)
    "owner UID is required" true
    (Result.is_error (K.Core.make_owner_reference widget_api missing_uid))

let kubeconfig_yaml =
  {|apiVersion: v1
kind: Config
clusters:
- cluster:
    certificate-authority-data: Q0E=
    server: https://127.0.0.1:6443
  name: local
contexts:
- context:
    cluster: local
    namespace: operators
    user: admin
  name: local
current-context: local
users:
- name: admin
  user:
    client-certificate-data: Q0VSVA==
    client-key-data: S0VZ
|}

let with_temp_file contents fn =
  let path = Filename.temp_file "kube-test-" ".yaml" in
  let channel = open_out_bin path in
  output_string channel contents;
  close_out channel;
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () -> fn path)

let write_file path contents =
  let channel = open_out_bin path in
  output_string channel contents;
  close_out channel

let with_temp_directory fn =
  let path = Filename.temp_dir "kube-test-" "" in
  let files = ref [] in
  let create name contents =
    let path = Filename.concat path name in
    write_file path contents;
    files := path :: !files;
    path
  in
  Fun.protect
    ~finally:(fun () ->
      List.iter Sys.remove !files;
      Unix.rmdir path)
    (fun () -> fn create)

let test_kubeconfig () =
  with_temp_file kubeconfig_yaml (fun path ->
      match K.Config.load_kubeconfig path with
      | Error message -> Alcotest.fail message
      | Ok config ->
          Alcotest.(check string)
            "server" "https://127.0.0.1:6443"
            (Uri.to_string config.server);
          Alcotest.(check (option string))
            "namespace" (Some "operators") config.namespace;
          Alcotest.(check (option string))
            "CA data" (Some "CA") config.tls.ca_pem;
          Alcotest.(check (option string))
            "client cert" (Some "CERT") config.tls.client_certificate_pem;
          Alcotest.(check (option string))
            "client key" (Some "KEY") config.tls.client_key_pem)

let test_merged_kubeconfigs () =
  with_temp_directory (fun create ->
      let ca_path = create "ca.pem" "MERGED CA" in
      let token_path = create "token" "merged-token\n" in
      let context =
        create "context.yaml"
          {|apiVersion: v1
kind: Config
current-context: merged
contexts:
- name: merged
  context:
    cluster: separate-cluster
    namespace: merged-ns
    user: separate-user
|}
      in
      let cluster =
        create "cluster.yaml"
          (Printf.sprintf
             {|apiVersion: v1
kind: Config
clusters:
- name: separate-cluster
  cluster:
    server: https://merged.example.invalid
    certificate-authority: %s
|}
             (Filename.basename ca_path))
      in
      let user =
        create "user.yaml"
          (Printf.sprintf
             {|apiVersion: v1
kind: Config
users:
- name: separate-user
  user:
    token: stale-inline-token
    tokenFile: %s
|}
             (Filename.basename token_path))
      in
      match K.Config.load_kubeconfigs [ context; cluster; user ] with
      | Error message -> Alcotest.fail message
      | Ok config -> (
          Alcotest.(check (option string))
            "merged namespace" (Some "merged-ns") config.namespace;
          Alcotest.(check (option string))
            "relative CA" (Some "MERGED CA") config.tls.ca_pem;
          match K.Config.authorization_header config with
          | Error message -> Alcotest.fail message
          | Ok value ->
              Alcotest.(check (option string))
                "relative token" (Some "Bearer merged-token") value))

let test_kubeconfig_proxy_and_impersonation () =
  let document =
    {|apiVersion: v1
kind: Config
clusters:
- name: proxied
  cluster:
    server: https://kubernetes.example.test
    proxy-url: http://proxy-user:proxy-password@proxy.example.test:3128
contexts:
- name: proxied
  context:
    cluster: proxied
    user: operator
current-context: proxied
users:
- name: operator
  user:
    as: alice@example.test
    as-uid: user-42
    as-groups:
    - developers
    - auditors
    as-user-extra:
      example.com/scope:
      - read
      - write
|}
  in
  with_temp_file document (fun path ->
      match K.Config.load_kubeconfig path with
      | Error message -> Alcotest.fail message
      | Ok config ->
          Alcotest.(check (option string))
            "proxy URL"
            (Some "http://proxy-user:proxy-password@proxy.example.test:3128")
            (Option.map Uri.to_string config.proxy_url);
          Alcotest.(check (list (pair string string)))
            "impersonation headers"
            [
              ("Impersonate-User", "alice@example.test");
              ("Impersonate-Uid", "user-42");
              ("Impersonate-Group", "developers");
              ("Impersonate-Group", "auditors");
              ("Impersonate-Extra-example.com%2Fscope", "read");
              ("Impersonate-Extra-example.com%2Fscope", "write");
            ]
            (K.Config.impersonation_headers config));
  Alcotest.check_raises "reject proxy query"
    (Invalid_argument "proxy URL must not contain a path, query, or fragment")
    (fun () ->
      ignore
        (K.Config.make
           ~proxy_url:(Uri.of_string "http://proxy.example.test?route=cluster")
           (Uri.of_string "https://kubernetes.example.test")))

let test_queue_deduplication () =
  let queue = K.Work_queue.create () in
  let key = K.Core.Object_key.make ~namespace:"default" "sample" in
  K.Work_queue.add queue key;
  K.Work_queue.add queue key;
  Alcotest.(check int) "one ready key" 1 (K.Work_queue.length queue);
  let taken = K.Work_queue.take queue in
  Alcotest.(check bool) "take key" true (taken = Some key);
  K.Work_queue.add queue key;
  K.Work_queue.add queue key;
  Alcotest.(check int) "dirty but not concurrent" 0 (K.Work_queue.length queue);
  K.Work_queue.task_done queue key;
  Alcotest.(check int)
    "queued once after completion" 1
    (K.Work_queue.length queue);
  ignore (K.Work_queue.take queue);
  K.Work_queue.task_done queue key;
  Alcotest.(check int)
    "empty after final completion" 0
    (K.Work_queue.length queue);
  K.Work_queue.close queue

let test_scheduler_shutdown () =
  let cancel = K.Cancel.create () in
  let queue = K.Work_queue.create () in
  let scheduler = K.Work_queue.Scheduler.create ~cancel queue in
  K.Work_queue.Scheduler.stop scheduler;
  K.Work_queue.Scheduler.stop scheduler;
  K.Work_queue.Scheduler.schedule scheduler ~after:0.0
    (K.Core.Object_key.make "after-stop");
  Alcotest.(check int)
    "stopped scheduler ignores new work" 0
    (K.Work_queue.length queue);
  K.Work_queue.close queue

let test_controller_error_policy () =
  let key = K.Core.Object_key.make ~namespace:"default" "widget" in
  let policy =
    K.Controller.exponential_backoff ~initial:0.5 ~maximum:2.0 ~max_retries:2 ()
  in
  let delay attempt =
    match policy ~key ~attempt ~error:"failure" with
    | K.Controller.Retry_after delay -> delay
    | K.Controller.Drop -> Alcotest.fail "retry was dropped too early"
  in
  Alcotest.(check (float 0.0)) "first retry" 0.5 (delay 0);
  Alcotest.(check (float 0.0)) "second retry" 1.0 (delay 1);
  Alcotest.(check bool)
    "retry budget exhausted" true
    (policy ~key ~attempt:2 ~error:"failure" = K.Controller.Drop);
  Alcotest.check_raises "negative retry budget rejected"
    (Invalid_argument
       "Controller.exponential_backoff: max_retries must not be negative")
    (fun () ->
      let invalid = K.Controller.exponential_backoff ~max_retries:(-1) () in
      ignore (invalid ~key ~attempt:0 ~error:"failure"))

let test_rate_limiter () =
  let limiter = K.Rate_limiter.create ~qps:20.0 ~burst:1 in
  Alcotest.(check bool)
    "initial burst token" true
    (K.Rate_limiter.acquire limiter);
  let started = Unix.gettimeofday () in
  Alcotest.(check bool) "refill token" true (K.Rate_limiter.acquire limiter);
  let elapsed = Unix.gettimeofday () -. started in
  Alcotest.(check bool) "second request was throttled" true (elapsed >= 0.035);
  let cancel = K.Cancel.create () in
  K.Cancel.cancel cancel;
  Alcotest.(check bool)
    "cancelled waiter" false
    (K.Rate_limiter.acquire ~cancel limiter);
  Alcotest.(check bool)
    "unlimited accepts immediately" true
    (K.Rate_limiter.acquire K.Rate_limiter.unlimited);
  Alcotest.check_raises "invalid qps rejected"
    (Invalid_argument "Rate_limiter.create: qps must be finite and positive")
    (fun () -> ignore (K.Rate_limiter.create ~qps:0.0 ~burst:1))

let fixed_log_time () =
  match Ptime.of_rfc3339 "2026-08-30T12:34:56Z" with
  | Ok (time, _, _) -> time
  | Error _ -> Alcotest.fail "invalid fixed log timestamp"

let log_string_field name (event : K.Log.event) =
  match List.assoc_opt name event.fields with
  | Some (K.Log.String value) -> Some value
  | Some _ | None -> None

let test_structured_logging () =
  let fixed = fixed_log_time () in
  let captured = ref [] in
  let logger =
    K.Log.create ~min_level:K.Log.Debug
      ~now:(fun () -> fixed)
      ~sink:(fun event -> captured := event :: !captured)
      ()
  in
  let contextual =
    K.Log.with_fields logger
      [
        ("service", K.Log.String "operator");
        ("override", K.Log.String "inherited");
      ]
  in
  K.Log.debug contextual
    ~fields:[ ("token", K.Log.Redacted); ("override", K.Log.String "local") ]
    "Reconciliation started";
  let event =
    match !captured with
    | [ event ] -> event
    | events ->
        Alcotest.failf "expected one structured event, got %d"
          (List.length events)
  in
  Alcotest.(check string) "level" "debug" (K.Log.level_to_string event.level);
  Alcotest.(check string) "message" "Reconciliation started" event.message;
  Alcotest.(check (list string))
    "fields are deterministic and local values win"
    [ "override"; "service"; "token" ]
    (List.map fst event.fields);
  Alcotest.(check (option string))
    "local override" (Some "local")
    (log_string_field "override" event);
  let encoded = K.Log.event_to_yojson event in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "fixed timestamp" "2026-08-30T12:34:56.000000Z"
    (encoded |> member "timestamp" |> to_string);
  Alcotest.(check string)
    "redaction" "[REDACTED]"
    (encoded |> member "token" |> to_string);
  K.Log.set_min_level contextual K.Log.Warn;
  K.Log.info logger "filtered";
  K.Log.error logger "retained";
  Alcotest.(check int)
    "shared level filters derived loggers" 2 (List.length !captured);
  let failing = K.Log.create ~sink:(fun _ -> failwith "sink failed") () in
  K.Log.info failing "isolated";
  Alcotest.(check int)
    "sink exception is counted" 1
    (K.Log.dropped_events failing);
  Alcotest.check_raises "reserved field rejected"
    (Invalid_argument "Log: reserved field name message") (fun () ->
      ignore (K.Log.with_fields logger [ ("message", K.Log.String "reserved") ]));
  Alcotest.check_raises "duplicate field rejected"
    (Invalid_argument "Log: duplicate field duplicate") (fun () ->
      ignore
        (K.Log.with_fields logger
           [ ("duplicate", K.Log.Int 1); ("duplicate", K.Log.Int 2) ]));
  Alcotest.check_raises "non-finite field rejected"
    (Invalid_argument "Log: non-finite field bad") (fun () ->
      K.Log.error logger
        ~fields:[ ("bad", K.Log.Float Float.infinity) ]
        "invalid");
  let active = Atomic.make false in
  let overlap = Atomic.make false in
  let emitted = Atomic.make 0 in
  let concurrent =
    K.Log.create ~min_level:K.Log.Debug
      ~now:(fun () -> fixed)
      ~sink:(fun _ ->
        if not (Atomic.compare_and_set active false true) then
          Atomic.set overlap true;
        Thread.yield ();
        Atomic.incr emitted;
        Atomic.set active false)
      ()
  in
  let threads =
    List.init 4 (fun worker ->
        Thread.create
          (fun () ->
            for sequence = 1 to 50 do
              K.Log.debug concurrent
                ~fields:
                  [
                    ("worker", K.Log.Int worker);
                    ("sequence", K.Log.Int sequence);
                  ]
                "concurrent"
            done)
          ())
  in
  List.iter Thread.join threads;
  Alcotest.(check int) "all concurrent events emitted" 200 (Atomic.get emitted);
  Alcotest.(check bool) "sink calls are serialized" false (Atomic.get overlap)

let test_logging_integration () =
  let captured = ref [] in
  let logger =
    K.Log.create ~min_level:K.Log.Debug ~now:fixed_log_time
      ~sink:(fun event -> captured := event :: !captured)
      ()
  in
  Fake_api_server.with_server
    [ Fake_api_server.fixed "{}" ]
    (fun server ->
      let client = K.Client.create ~logger (Fake_api_server.config server) in
      Fun.protect
        ~finally:(fun () -> K.Client.close client)
        (fun () ->
          (match
             K.Client.raw client `GET
               "/version?resourceVersion=sensitive-query-value"
           with
          | Ok _ -> ()
          | Error error ->
              Alcotest.fail (Format.asprintf "%a" K.Client.pp_error error));
          let component =
            K.Manager.component ~name:"logging-proof"
              (fun ~client:_ ~cancel:_ -> Ok ())
          in
          let manager = K.Manager.create client in
          K.Manager.add manager component;
          match K.Manager.run manager with
          | Ok () -> ()
          | Error error ->
              Alcotest.fail (Format.asprintf "%a" K.Manager.pp_error error)));
  let find message =
    match
      List.find_opt
        (fun (event : K.Log.event) -> event.message = message)
        !captured
    with
    | Some event -> event
    | None -> Alcotest.fail ("missing log event: " ^ message)
  in
  let started = find "Kubernetes API request started" in
  Alcotest.(check (option string))
    "request log strips query values" (Some "/version")
    (log_string_field "path" started);
  let rendered =
    List.map K.Log.event_to_yojson !captured |> fun values ->
    Yojson.Safe.to_string (`List values)
  in
  let contains value substring =
    let rec loop index =
      if index + String.length substring > String.length value then false
      else if String.sub value index (String.length substring) = substring then
        true
      else loop (index + 1)
    in
    loop 0
  in
  Alcotest.(check bool)
    "query value is not logged" false
    (contains rendered "sensitive-query-value");
  let manager = find "Controller manager starting" in
  Alcotest.(check (option string))
    "manager logger context" (Some "manager")
    (log_string_field "logger" manager);
  ignore (find "Manager component starting");
  ignore (find "Controller manager stopped")

let test_cached_client_readiness () =
  let config = K.Config.make (Uri.of_string "http://127.0.0.1:1") in
  let client = K.Client.create config in
  Fun.protect
    ~finally:(fun () -> K.Client.close client)
    (fun () ->
      let cache = Widget_controller.Cache.create () in
      let reader = Widget_controller.Cached.make ~client ~cache in
      Alcotest.(check bool)
        "reader retains live client" true
        (Widget_controller.Cached.client reader == client);
      let check_not_ready = function
        | Error (K.Client.Transport "cache is not synchronized") -> ()
        | Error error ->
            Alcotest.fail
              ("unexpected unsynchronized-cache error: "
              ^ Format.asprintf "%a" K.Client.pp_error error)
        | Ok _ -> Alcotest.fail "unsynchronized cache returned data"
      in
      check_not_ready
        (Widget_controller.Cached.get reader ~namespace:"default" "missing");
      check_not_ready (Widget_controller.Cached.list reader))

let test_manager_dependency_deduplication () =
  let config = K.Config.make (Uri.of_string "http://127.0.0.1:1") in
  let client = K.Client.create config in
  Fun.protect
    ~finally:(fun () -> K.Client.close client)
    (fun () ->
      let dependency_runs = Atomic.make 0 in
      let child_runs = Atomic.make 0 in
      let dependency =
        K.Manager.component ~name:"shared-dependency"
          (fun ~client:_ ~cancel:_ ->
            Atomic.incr dependency_runs;
            Ok ())
      in
      let child name =
        K.Manager.component ~name ~dependencies:[ dependency ]
          (fun ~client:_ ~cancel:_ ->
            Atomic.incr child_runs;
            Ok ())
      in
      let manager = K.Manager.create client in
      K.Manager.add manager (child "first");
      K.Manager.add manager (child "second");
      K.Manager.add manager dependency;
      (match K.Manager.run manager with
      | Ok () -> ()
      | Error error ->
          Alcotest.fail (Format.asprintf "%a" K.Manager.pp_error error));
      Alcotest.(check int)
        "dependency started once" 1
        (Atomic.get dependency_runs);
      Alcotest.(check int) "both children started" 2 (Atomic.get child_runs))

let test_manager_failure_cancels_siblings () =
  let config = K.Config.make (Uri.of_string "http://127.0.0.1:1") in
  let client = K.Client.create config in
  Fun.protect
    ~finally:(fun () -> K.Client.close client)
    (fun () ->
      let sibling_stopped = Atomic.make false in
      let sibling =
        K.Manager.component ~name:"sibling" (fun ~client:_ ~cancel ->
            while not (K.Cancel.is_cancelled cancel) do
              ignore (K.Cancel.sleep cancel 0.01)
            done;
            Atomic.set sibling_stopped true;
            Ok ())
      in
      let failure =
        K.Manager.component ~name:"failure" (fun ~client:_ ~cancel:_ ->
            Error (K.Client.Invalid_request "intentional failure"))
      in
      let manager = K.Manager.create client in
      K.Manager.add manager sibling;
      K.Manager.add manager failure;
      (match K.Manager.run manager with
      | Error
          {
            K.Manager.component = "failure";
            cause = K.Client.Invalid_request "intentional failure";
          } -> ()
      | Error error ->
          Alcotest.fail
            ("unexpected manager error: "
            ^ Format.asprintf "%a" K.Manager.pp_error error)
      | Ok () -> Alcotest.fail "manager ignored a component failure");
      Alcotest.(check bool)
        "sibling joined after cancellation" true
        (Atomic.get sibling_stopped))

let write_all fd value =
  let rec loop offset =
    if offset < String.length value then
      let count =
        Unix.write_substring fd value offset (String.length value - offset)
      in
      loop (offset + count)
  in
  loop 0

let read_request fd =
  let buffer = Bytes.create 4096 in
  let output = Buffer.create 4096 in
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
    let count = Unix.read fd buffer 0 (Bytes.length buffer) in
    if count = 0 then Buffer.contents output
    else (
      Buffer.add_subbytes output buffer 0 count;
      loop ())
  in
  loop ()

let response ?(status = "200 OK") body =
  Printf.sprintf
    "HTTP/1.1 %s\r\n\
     Content-Type: application/json\r\n\
     Content-Length: %d\r\n\
     Connection: close\r\n\
     \r\n\
     %s"
    status (String.length body) body

let contains_substring value substring =
  let rec loop index =
    if String.length substring = 0 then true
    else if index + String.length substring > String.length value then false
    else if String.sub value index (String.length substring) = substring then
      true
    else loop (index + 1)
  in
  loop 0

let test_metrics_registry () =
  let registry = K.Metrics.create () in
  let labels = [ ("controller", "greeting") ] in
  let active =
    K.Metrics.Gauge.create ~registry ~name:"ocaml_k8s_active_workers"
      ~help:"Active reconciliation workers." ~labels ()
  in
  let reconciliations =
    K.Metrics.Counter.create ~registry ~name:"ocaml_k8s_reconciliations_total"
      ~help:"Completed reconciliation attempts." ~labels ()
  in
  let duration =
    K.Metrics.Histogram.create ~registry
      ~name:"ocaml_k8s_reconcile_duration_seconds"
      ~help:"Reconciliation latency." ~buckets:[ 1.; 5. ] ~labels ()
  in
  K.Metrics.Gauge.set active 2.;
  K.Metrics.Counter.add reconciliations 3.;
  List.iter (K.Metrics.Histogram.observe duration) [ 0.5; 3.; 7. ];
  let expected =
    {|# HELP ocaml_k8s_active_workers Active reconciliation workers.
# TYPE ocaml_k8s_active_workers gauge
ocaml_k8s_active_workers{controller="greeting"} 2
# HELP ocaml_k8s_reconcile_duration_seconds Reconciliation latency.
# TYPE ocaml_k8s_reconcile_duration_seconds histogram
ocaml_k8s_reconcile_duration_seconds_bucket{controller="greeting",le="1"} 1
ocaml_k8s_reconcile_duration_seconds_bucket{controller="greeting",le="5"} 2
ocaml_k8s_reconcile_duration_seconds_bucket{controller="greeting",le="+Inf"} 3
ocaml_k8s_reconcile_duration_seconds_sum{controller="greeting"} 10.5
ocaml_k8s_reconcile_duration_seconds_count{controller="greeting"} 3
# HELP ocaml_k8s_reconciliations_total Completed reconciliation attempts.
# TYPE ocaml_k8s_reconciliations_total counter
ocaml_k8s_reconciliations_total{controller="greeting"} 3
|}
  in
  Alcotest.(check string)
    "Prometheus exposition" expected
    (K.Metrics.render registry)

let http_get port path =
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> try Unix.close fd with Unix.Unix_error _ -> ())
    (fun () ->
      Unix.connect fd (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
      write_all fd
        ("GET " ^ path
       ^ " HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n");
      let output = Buffer.create 1024 in
      let buffer = Bytes.create 1024 in
      let rec read () =
        let count = Unix.read fd buffer 0 (Bytes.length buffer) in
        if count > 0 then (
          Buffer.add_subbytes output buffer 0 count;
          read ())
      in
      read ();
      Buffer.contents output)

let test_diagnostics_server () =
  let config = K.Config.make (Uri.of_string "http://127.0.0.1:1") in
  let log_events = ref [] in
  let logger =
    K.Log.create ~sink:(fun event -> log_events := event :: !log_events) ()
  in
  let client = K.Client.create ~logger config in
  Fun.protect
    ~finally:(fun () -> K.Client.close client)
    (fun () ->
      let health = K.Health.create () in
      let ready = Atomic.make false in
      let _remove_liveness =
        K.Health.add_liveness health ~name:"process" (fun () -> Ok ())
      in
      let _remove_readiness =
        K.Health.add_readiness health ~name:"cache" (fun () ->
            if Atomic.get ready then Ok () else Error "not synchronized")
      in
      let metrics = K.Metrics.create () in
      let requests =
        K.Metrics.Counter.create ~registry:metrics
          ~name:"ocaml_k8s_test_requests_total" ~help:"Test requests." ()
      in
      K.Metrics.Counter.inc requests;
      let diagnostics = K.Diagnostics.create ~port:0 ~health ~metrics () in
      let cancel = K.Cancel.create () in
      let result = ref None in
      let manager_thread =
        Thread.create
          (fun () ->
            let manager = K.Manager.create ~cancel client in
            K.Manager.add manager (K.Diagnostics.component diagnostics);
            result := Some (K.Manager.run manager))
          ()
      in
      let port =
        match K.Diagnostics.await_listening ~cancel diagnostics with
        | Ok port -> port
        | Error message -> Alcotest.fail message
      in
      let live = http_get port "/healthz" in
      Alcotest.(check bool)
        "liveness succeeds" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" live);
      let not_ready = http_get port "/readyz" in
      Alcotest.(check bool)
        "readiness fails" true
        (String.starts_with ~prefix:"HTTP/1.1 503 Service Unavailable" not_ready);
      Alcotest.(check bool)
        "failed check is named" true
        (contains_substring not_ready "[-]cache failed: not synchronized");
      Atomic.set ready true;
      let ready_response = http_get port "/readyz" in
      Alcotest.(check bool)
        "readiness recovers" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" ready_response);
      let metrics_response = http_get port "/metrics" in
      Alcotest.(check bool)
        "metrics content type" true
        (contains_substring metrics_response K.Metrics.content_type);
      Alcotest.(check bool)
        "metrics rendered" true
        (contains_substring metrics_response "ocaml_k8s_test_requests_total 1");
      K.Cancel.cancel cancel;
      Thread.join manager_thread;
      (match !result with
      | Some (Ok ()) -> ()
      | Some (Error error) ->
          Alcotest.fail (Format.asprintf "%a" K.Manager.pp_error error)
      | None -> Alcotest.fail "diagnostics manager produced no result");
      let logged message =
        List.exists
          (fun (event : K.Log.event) -> event.message = message)
          !log_events
      in
      Alcotest.(check bool)
        "listening lifecycle logged" true
        (logged "Diagnostics server listening");
      Alcotest.(check bool)
        "shutdown lifecycle logged" true
        (logged "Diagnostics server stopped"))

let widget_json ?(namespace = "default") ?(labels = []) ~name ~resource_version
    () =
  `Assoc
    [
      ("apiVersion", `String "testing.ocaml-k8s.dev/v1");
      ("kind", `String "Widget");
      ( "metadata",
        `Assoc
          ([
             ("name", `String name);
             ("namespace", `String namespace);
             ("resourceVersion", `String resource_version);
           ]
          @ if labels = [] then [] else [ ("labels", `Assoc labels) ]) );
      ("spec", `Assoc []);
    ]

let list_json ?continue ?remaining ~resource_version items =
  let optional name fn = function
    | None -> []
    | Some value -> [ (name, fn value) ]
  in
  `Assoc
    [
      ("apiVersion", `String "testing.ocaml-k8s.dev/v1");
      ("kind", `String "WidgetList");
      ( "metadata",
        `Assoc
          ([ ("resourceVersion", `String resource_version) ]
          @ optional "continue" (fun value -> `String value) continue
          @ optional "remainingItemCount" (fun value -> `Int value) remaining)
      );
      ("items", `List items);
    ]

let test_store_indexes () =
  let resource name tier resource_version =
    widget_json ~labels:[ ("tier", `String tier) ] ~name ~resource_version ()
    |> K.Dynamic.of_json
    |> function
    | Ok value -> value
    | Error message -> Alcotest.fail message
  in
  let names values =
    values
    |> List.sort (fun left right ->
        K.Core.Object_key.compare
          (K.Core.key_of_meta left.K.Dynamic.metadata)
          (K.Core.key_of_meta right.K.Dynamic.metadata))
    |> List.map (fun value -> value.K.Dynamic.metadata.name)
  in
  let store = Widget_store.create () in
  (match
     Widget_store.add_index store ~name:"tier" (fun value ->
         match List.assoc_opt "tier" value.K.Dynamic.metadata.labels with
         | Some tier -> [ tier; tier ]
         | None -> [])
   with
  | Ok () -> ()
  | Error message -> Alcotest.fail message);
  let frontend = resource "a" "frontend" "1" in
  let backend = resource "b" "backend" "1" in
  ignore (Widget_store.upsert store frontend);
  ignore (Widget_store.upsert store backend);
  let lookup value =
    match Widget_store.by_index store ~name:"tier" value with
    | Ok values -> names values
    | Error message -> Alcotest.fail message
  in
  Alcotest.(check (list string)) "initial index" [ "a" ] (lookup "frontend");
  ignore (Widget_store.upsert store (resource "a" "backend" "2"));
  Alcotest.(check (list string)) "old value removed" [] (lookup "frontend");
  Alcotest.(check (list string))
    "updated value added once" [ "a"; "b" ] (lookup "backend");
  ignore
    (Widget_store.remove store
       (K.Core.Object_key.make ~namespace:"default" "b"));
  Alcotest.(check (list string))
    "remove updates index" [ "a" ] (lookup "backend");
  ignore (Widget_store.replace store [ resource "b" "frontend" "3" ]);
  Alcotest.(check (list string))
    "replace clears old buckets" [] (lookup "backend");
  Alcotest.(check (list string))
    "replace rebuilds buckets" [ "b" ] (lookup "frontend");
  Alcotest.(check bool)
    "duplicate index rejected" true
    (Result.is_error (Widget_store.add_index store ~name:"tier" (fun _ -> [])));
  Alcotest.(check bool)
    "unknown index rejected" true
    (Result.is_error (Widget_store.by_index store ~name:"missing" "value"))

let test_controller_cache_sync_timeout () =
  Fake_api_server.with_server
    [ Fake_api_server.raw [] ]
    (fun server ->
      let client = K.Client.create (Fake_api_server.config server) in
      Fun.protect
        ~finally:(fun () -> K.Client.close client)
        (fun () ->
          let reconciled = Atomic.make false in
          let started = Unix.gettimeofday () in
          let result =
            Widget_controller.run ~cache_sync_timeout:0.05 client
              ~reconcile:(fun _client _request ->
                Atomic.set reconciled true;
                Ok K.Controller.Done)
          in
          let elapsed = Unix.gettimeofday () -. started in
          (match result with
          | Error (K.Client.Transport message) ->
              Alcotest.(check bool)
                "timeout is identified" true
                (contains_substring message
                   "cache synchronization timed out after")
          | Error error ->
              Alcotest.fail
                ("unexpected cache-sync error: "
                ^ Format.asprintf "%a" K.Client.pp_error error)
          | Ok () -> Alcotest.fail "unsynchronized controller started");
          Alcotest.(check bool)
            "reconciler never started" false (Atomic.get reconciled);
          Alcotest.(check bool) "cache timeout is bounded" true (elapsed < 1.0)))

let test_controller_reconcile_timeout () =
  let listed =
    list_json ~resource_version:"91"
      [ widget_json ~name:"slow" ~resource_version:"91" () ]
    |> Yojson.Safe.to_string
  in
  Fake_api_server.with_server
    [ Fake_api_server.fixed listed; Fake_api_server.chunked [] ]
    (fun server ->
      let client = K.Client.create (Fake_api_server.config server) in
      Fun.protect
        ~finally:(fun () -> K.Client.close client)
        (fun () ->
          let cancel = K.Cancel.create () in
          let metrics = K.Metrics.create () in
          let child_token = Atomic.make false in
          let deadline_observed = Atomic.make false in
          let timeout_classified = Atomic.make false in
          let started = Unix.gettimeofday () in
          let reconcile _client request =
            Atomic.set child_token (request.Widget_controller.cancel != cancel);
            if K.Cancel.sleep request.Widget_controller.cancel 5.0 then
              Alcotest.fail "reconciliation deadline did not cancel its token";
            Atomic.set deadline_observed true;
            K.Cancel.cancel cancel;
            Ok K.Controller.Done
          in
          let error_policy ~key:_ ~attempt:_ ~error =
            Atomic.set timeout_classified
              (contains_substring error "reconciliation timed out after");
            K.Controller.Drop
          in
          (match
             Widget_controller.run ~cancel ~name:"timeout-test" ~metrics
               ~reconcile_timeout:0.05 ~error_policy client ~reconcile
           with
          | Ok () -> ()
          | Error error ->
              Alcotest.fail
                ("unexpected controller error: "
                ^ Format.asprintf "%a" K.Client.pp_error error));
          let elapsed = Unix.gettimeofday () -. started in
          Alcotest.(check bool)
            "reconcile receives a child token" true (Atomic.get child_token);
          Alcotest.(check bool)
            "reconciler observes deadline" true
            (Atomic.get deadline_observed);
          Alcotest.(check bool)
            "deadline becomes retry-policy error" true
            (Atomic.get timeout_classified);
          Alcotest.(check bool)
            "reconcile timeout is bounded" true (elapsed < 1.0);
          Alcotest.(check bool)
            "timeout metric" true
            (contains_substring (K.Metrics.render metrics)
               "ocaml_k8s_reconciliations_total{controller=\"timeout-test\",result=\"timeout\"} \
                1")))

let request_target request =
  match String.split_on_char '\n' request with
  | request_line :: _ -> (
      match String.split_on_char ' ' (String.trim request_line) with
      | _method :: target :: _ -> target
      | _ -> Alcotest.fail "invalid captured request line")
  | [] -> Alcotest.fail "empty captured request"

let request_method request =
  match String.split_on_char '\n' request with
  | request_line :: _ -> (
      match String.split_on_char ' ' (String.trim request_line) with
      | meth :: _ -> meth
      | [] -> Alcotest.fail "invalid captured request line")
  | [] -> Alcotest.fail "empty captured request"

let request_body request =
  let marker = "\r\n\r\n" in
  let rec find index =
    if index + String.length marker > String.length request then None
    else if String.sub request index (String.length marker) = marker then
      Some (index + String.length marker)
    else find (index + 1)
  in
  match find 0 with
  | None -> Alcotest.fail "captured request has no header terminator"
  | Some start -> String.sub request start (String.length request - start)

let test_cached_client_live_delegation () =
  let resource version =
    widget_json ~name:"cached-writes" ~resource_version:version ()
    |> Yojson.Safe.to_string
  in
  Fake_api_server.with_server
    [
      Fake_api_server.fixed (resource "93");
      Fake_api_server.fixed (resource "94");
    ]
    (fun server ->
      let client = K.Client.create (Fake_api_server.config server) in
      Fun.protect
        ~finally:(fun () -> K.Client.close client)
        (fun () ->
          let cache = Widget_controller.Cache.create () in
          let reader = Widget_controller.Cached.make ~client ~cache in
          let patch = K.Client.Merge_patch (`Assoc [ ("spec", `Assoc []) ]) in
          (match
             Widget_controller.Cached.patch reader ~namespace:"default"
               "cached-writes" patch
           with
          | Ok value ->
              Alcotest.(check (option string))
                "live patch response" (Some "93")
                (Option.map K.Core.Resource_version.to_string
                   value.K.Dynamic.metadata.resource_version)
          | Error error ->
              Alcotest.fail
                ("cached-client PATCH failed: "
                ^ Format.asprintf "%a" K.Client.pp_error error));
          (match
             Widget_controller.Cached.fresh_get reader ~namespace:"default"
               "cached-writes"
           with
          | Ok value ->
              Alcotest.(check (option string))
                "explicit fresh response" (Some "94")
                (Option.map K.Core.Resource_version.to_string
                   value.K.Dynamic.metadata.resource_version)
          | Error error ->
              Alcotest.fail
                ("cached-client fresh GET failed: "
                ^ Format.asprintf "%a" K.Client.pp_error error));
          Alcotest.(check (list string))
            "writes and fresh reads use HTTP" [ "PATCH"; "GET" ]
            (List.map request_method (Fake_api_server.requests server))))

let discovery_resource ?(singular_name = "") ?(verbs = []) ?(short_names = [])
    ?(categories = []) ~name ~namespaced ~kind () =
  `Assoc
    [
      ("name", `String name);
      ("singularName", `String singular_name);
      ("namespaced", `Bool namespaced);
      ("kind", `String kind);
      ("verbs", `List (List.map (fun value -> `String value) verbs));
      ("shortNames", `List (List.map (fun value -> `String value) short_names));
      ("categories", `List (List.map (fun value -> `String value) categories));
    ]

let resource_discovery_document group_version resources =
  `Assoc
    [ ("groupVersion", `String group_version); ("resources", `List resources) ]
  |> Yojson.Safe.to_string

let apps_discovery_document () =
  resource_discovery_document "apps/v1"
    [
      discovery_resource ~singular_name:"deployment"
        ~verbs:[ "create"; "delete"; "get"; "list"; "patch"; "update"; "watch" ]
        ~short_names:[ "deploy" ] ~categories:[ "all" ] ~name:"deployments"
        ~namespaced:true ~kind:"Deployment" ();
      discovery_resource
        ~verbs:[ "get"; "patch"; "update" ]
        ~name:"deployments/status" ~namespaced:true ~kind:"Deployment" ();
      discovery_resource
        ~verbs:[ "get"; "patch"; "update" ]
        ~name:"deployments/scale" ~namespaced:true ~kind:"Scale" ();
    ]

let test_discovery_mapper_exact () =
  let document = apps_discovery_document () in
  Fake_api_server.with_server
    [ Fake_api_server.fixed document; Fake_api_server.fixed document ]
    (fun server ->
      let client = K.Client.create (Fake_api_server.config server) in
      Fun.protect
        ~finally:(fun () -> K.Client.close client)
        (fun () ->
          let mapper = K.Discovery.Mapper.create client in
          Alcotest.(check bool)
            "mapper retains client" true
            (K.Discovery.Mapper.client mapper == client);
          (match
             K.Discovery.Mapper.resolve_gvk mapper ~group:"apps" ~version:""
               ~kind:"Deployment"
           with
          | Error (K.Client.Invalid_request _) -> ()
          | Error error ->
              Alcotest.fail
                ("unexpected empty-version error: "
                ^ Format.asprintf "%a" K.Client.pp_error error)
          | Ok _ -> Alcotest.fail "empty discovery version was accepted");
          Alcotest.(check int)
            "invalid mapping performs no request" 0
            (List.length (Fake_api_server.requests server));
          let deployment =
            match
              K.Discovery.Mapper.resolve_gvk mapper ~group:"apps" ~version:"v1"
                ~kind:"Deployment"
            with
            | Ok mapping -> mapping
            | Error error ->
                Alcotest.fail
                  ("GVK discovery failed: "
                  ^ Format.asprintf "%a" K.Client.pp_error error)
          in
          Alcotest.(check string)
            "resolved plural" "deployments" deployment.api.plural;
          Alcotest.(check bool)
            "resolved scope" true
            (deployment.api.scope = K.Core.Namespaced);
          Alcotest.(check (list string))
            "short names" [ "deploy" ] deployment.short_names;
          Alcotest.(check (list string))
            "subresources" [ "scale"; "status" ]
            (List.map
               (fun subresource -> subresource.K.Discovery.Mapper.name)
               deployment.subresources);
          let by_resource =
            match
              K.Discovery.Mapper.resolve_gvr mapper ~group:"apps" ~version:"v1"
                ~resource:"deployments"
            with
            | Ok mapping -> mapping
            | Error error ->
                Alcotest.fail
                  ("GVR discovery failed: "
                  ^ Format.asprintf "%a" K.Client.pp_error error)
          in
          Alcotest.(check string)
            "cached GVR kind" "Deployment" by_resource.api.kind;
          (match
             K.Discovery.Mapper.resolve_gvk mapper ~group:"apps" ~version:"v1"
               ~kind:"Missing"
           with
          | Error (K.Client.Invalid_request _) -> ()
          | Error error ->
              Alcotest.fail
                ("unexpected missing-GVK error: "
                ^ Format.asprintf "%a" K.Client.pp_error error)
          | Ok _ -> Alcotest.fail "missing GVK unexpectedly resolved");
          Alcotest.(check int)
            "exact lookups share discovery" 1
            (List.length (Fake_api_server.requests server));
          K.Discovery.Mapper.invalidate mapper;
          (match
             K.Discovery.Mapper.resolve_gvr mapper ~group:"apps" ~version:"v1"
               ~resource:"deployments"
           with
          | Ok _ -> ()
          | Error error ->
              Alcotest.fail
                ("post-invalidation discovery failed: "
                ^ Format.asprintf "%a" K.Client.pp_error error));
          Alcotest.(check (list string))
            "invalidation refetches exact group-version"
            [ "/apis/apps/v1"; "/apis/apps/v1" ]
            (List.map request_target (Fake_api_server.requests server))))

let test_discovery_mapper_preferred () =
  let groups =
    {|{"groups":[{"name":"apps","versions":[{"groupVersion":"apps/v1beta1","version":"v1beta1"},{"groupVersion":"apps/v1","version":"v1"}],"preferredVersion":{"groupVersion":"apps/v1","version":"v1"}}]}|}
  in
  let malformed_groups =
    {|{"groups":[{"name":"apps","versions":[{"groupVersion":"apps/v1","version":"v1"}],"preferredVersion":{"groupVersion":"wrong/v1","version":"v1"}}]}|}
  in
  Fake_api_server.with_server
    [
      Fake_api_server.fixed groups;
      Fake_api_server.fixed (apps_discovery_document ());
      Fake_api_server.fixed malformed_groups;
    ]
    (fun server ->
      let client = K.Client.create (Fake_api_server.config server) in
      Fun.protect
        ~finally:(fun () -> K.Client.close client)
        (fun () ->
          let mapper = K.Discovery.Mapper.create client in
          (match K.Discovery.Mapper.preferred_version mapper ~group:"apps" with
          | Ok version ->
              Alcotest.(check string) "preferred version" "v1" version
          | Error error ->
              Alcotest.fail
                ("preferred-version discovery failed: "
                ^ Format.asprintf "%a" K.Client.pp_error error));
          let by_kind =
            match
              K.Discovery.Mapper.resolve_kind ~group:"apps" mapper
                ~kind:"Deployment"
            with
            | Ok mapping -> mapping
            | Error error ->
                Alcotest.fail
                  ("preferred Kind discovery failed: "
                  ^ Format.asprintf "%a" K.Client.pp_error error)
          in
          Alcotest.(check string)
            "preferred Kind version" "v1" by_kind.api.version;
          Alcotest.(check string)
            "singular name is retained" "deployment" by_kind.singular_name;
          List.iter
            (fun alias ->
              match
                K.Discovery.Mapper.resolve_resource ~group:"apps" mapper
                  ~resource:alias
              with
              | Ok mapping ->
                  Alcotest.(check string)
                    ("resource alias " ^ alias)
                    "deployments" mapping.api.plural
              | Error error ->
                  Alcotest.fail
                    ("resource alias discovery failed: "
                    ^ Format.asprintf "%a" K.Client.pp_error error))
            [ "deployments"; "deployment"; "deploy" ];
          (match
             K.Discovery.Mapper.resolve_resource ~group:"apps" mapper
               ~resource:"missing"
           with
          | Error (K.Client.Invalid_request _) -> ()
          | Error error ->
              Alcotest.fail
                ("unexpected missing-alias error: "
                ^ Format.asprintf "%a" K.Client.pp_error error)
          | Ok _ -> Alcotest.fail "missing resource alias unexpectedly resolved");
          K.Discovery.Mapper.invalidate mapper;
          (match K.Discovery.Mapper.preferred_version mapper ~group:"apps" with
          | Error (K.Client.Decode message) ->
              Alcotest.(check bool)
                "malformed preferred G/V is rejected" true
                (contains_substring message "invalid groupVersion")
          | Error error ->
              Alcotest.fail
                ("unexpected malformed-preference error: "
                ^ Format.asprintf "%a" K.Client.pp_error error)
          | Ok _ ->
              Alcotest.fail "malformed preferred groupVersion was accepted");
          Alcotest.(check (list string))
            "preferred and alias discovery is cached and invalidatable"
            [ "/apis"; "/apis/apps/v1"; "/apis" ]
            (List.map request_target (Fake_api_server.requests server))))

let test_discovery_mapper_ambiguity () =
  let core_versions = {|{"versions":["v1"]}|} in
  let groups =
    {|{"groups":[{"name":"apps","versions":[{"groupVersion":"apps/v1","version":"v1"}],"preferredVersion":{"groupVersion":"apps/v1","version":"v1"}},{"name":"example.dev","versions":[{"groupVersion":"example.dev/v1","version":"v1"}],"preferredVersion":{"groupVersion":"example.dev/v1","version":"v1"}}]}|}
  in
  let core_resources =
    resource_discovery_document "v1"
      [
        discovery_resource ~singular_name:"pod" ~name:"pods" ~namespaced:true
          ~kind:"Pod" ();
      ]
  in
  let example_resources =
    resource_discovery_document "example.dev/v1"
      [
        discovery_resource ~singular_name:"deployment" ~short_names:[ "deploy" ]
          ~name:"deployments" ~namespaced:true ~kind:"Deployment" ();
      ]
  in
  Fake_api_server.with_server
    [
      Fake_api_server.fixed core_versions;
      Fake_api_server.fixed groups;
      Fake_api_server.fixed core_resources;
      Fake_api_server.fixed (apps_discovery_document ());
      Fake_api_server.fixed example_resources;
    ]
    (fun server ->
      let client = K.Client.create (Fake_api_server.config server) in
      Fun.protect
        ~finally:(fun () -> K.Client.close client)
        (fun () ->
          let mapper = K.Discovery.Mapper.create client in
          (match K.Discovery.Mapper.resolve_kind mapper ~kind:"Deployment" with
          | Error (K.Client.Invalid_request message) ->
              Alcotest.(check bool)
                "Kind ambiguity is explicit" true
                (contains_substring message "multiple resources")
          | Error error ->
              Alcotest.fail
                ("unexpected ambiguous-Kind error: "
                ^ Format.asprintf "%a" K.Client.pp_error error)
          | Ok _ -> Alcotest.fail "ambiguous Kind unexpectedly resolved");
          (match
             K.Discovery.Mapper.resolve_resource mapper ~resource:"deploy"
           with
          | Error (K.Client.Invalid_request _) -> ()
          | Error error ->
              Alcotest.fail
                ("unexpected ambiguous-alias error: "
                ^ Format.asprintf "%a" K.Client.pp_error error)
          | Ok _ -> Alcotest.fail "ambiguous short name unexpectedly resolved");
          (match
             K.Discovery.Mapper.resolve_kind ~group:"apps" mapper
               ~kind:"Deployment"
           with
          | Ok mapping ->
              Alcotest.(check string)
                "group disambiguates Kind" "apps" mapping.api.group
          | Error error ->
              Alcotest.fail
                ("group-qualified Kind failed: "
                ^ Format.asprintf "%a" K.Client.pp_error error));
          Alcotest.(check (list string))
            "unqualified preferred lookup visits each preferred group"
            [
              "/api";
              "/apis";
              "/api/v1";
              "/apis/apps/v1";
              "/apis/example.dev/v1";
            ]
            (List.map request_target (Fake_api_server.requests server))))

let test_discovery_mapper_all () =
  let core_versions = {|{"versions":["v1"]}|} in
  let groups =
    {|{"groups":[{"name":"apps","versions":[{"groupVersion":"apps/v1","version":"v1"}],"preferredVersion":{"groupVersion":"apps/v1","version":"v1"}}]}|}
  in
  let core_resources =
    resource_discovery_document "v1"
      [
        discovery_resource ~singular_name:"node" ~verbs:[ "get"; "list" ]
          ~name:"nodes" ~namespaced:false ~kind:"Node" ();
        discovery_resource ~singular_name:"pod" ~verbs:[ "get"; "list" ]
          ~name:"pods" ~namespaced:true ~kind:"Pod" ();
      ]
  in
  Fake_api_server.with_server
    [
      Fake_api_server.fixed core_versions;
      Fake_api_server.fixed groups;
      Fake_api_server.fixed core_resources;
      Fake_api_server.fixed (apps_discovery_document ());
      Fake_api_server.fixed core_versions;
      Fake_api_server.fixed groups;
      Fake_api_server.fixed core_resources;
      Fake_api_server.fixed (apps_discovery_document ());
    ]
    (fun server ->
      let client = K.Client.create (Fake_api_server.config server) in
      Fun.protect
        ~finally:(fun () -> K.Client.close client)
        (fun () ->
          let mapper = K.Discovery.Mapper.create client in
          let discover () =
            match K.Discovery.Mapper.mappings mapper with
            | Ok mappings -> mappings
            | Error error ->
                Alcotest.fail
                  ("complete discovery failed: "
                  ^ Format.asprintf "%a" K.Client.pp_error error)
          in
          let identifiers =
            discover ()
            |> List.map (fun (mapping : K.Discovery.Mapper.mapping) ->
                Printf.sprintf "%s/%s/%s" mapping.api.group mapping.api.version
                  mapping.api.plural)
          in
          Alcotest.(check (list string))
            "stable complete mappings"
            [ "/v1/nodes"; "/v1/pods"; "apps/v1/deployments" ]
            identifiers;
          ignore (discover ());
          Alcotest.(check (list string))
            "complete discovery is cached"
            [ "/api"; "/apis"; "/api/v1"; "/apis/apps/v1" ]
            (List.map request_target (Fake_api_server.requests server));
          (match K.Discovery.Mapper.refresh mapper with
          | Ok refreshed ->
              Alcotest.(check int)
                "refresh returns complete mappings" 3 (List.length refreshed)
          | Error error ->
              Alcotest.fail
                ("discovery refresh failed: "
                ^ Format.asprintf "%a" K.Client.pp_error error));
          Alcotest.(check (list string))
            "refresh replaces all cached discovery"
            [
              "/api";
              "/apis";
              "/api/v1";
              "/apis/apps/v1";
              "/api";
              "/apis";
              "/api/v1";
              "/apis/apps/v1";
            ]
            (List.map request_target (Fake_api_server.requests server))))

let test_discovery_mapper_concurrent () =
  Fake_api_server.with_server
    [
      Fake_api_server.fixed (apps_discovery_document ())
      |> Fake_api_server.delayed 0.05;
    ]
    (fun server ->
      let client = K.Client.create (Fake_api_server.config server) in
      Fun.protect
        ~finally:(fun () -> K.Client.close client)
        (fun () ->
          let mapper = K.Discovery.Mapper.create client in
          let request_cancel = K.Cancel.create () in
          let watchdog_stop = K.Cancel.create () in
          let start = Atomic.make false in
          let results = Array.make 2 None in
          let worker index =
            while not (Atomic.get start) do
              Thread.yield ()
            done;
            results.(index) <-
              Some
                (K.Discovery.Mapper.resolve_gvk ~cancel:request_cancel mapper
                   ~group:"apps" ~version:"v1" ~kind:"Deployment")
          in
          let workers = List.init 2 (fun index -> Thread.create worker index) in
          let watchdog =
            Thread.create
              (fun () ->
                if K.Cancel.sleep watchdog_stop 1.0 then
                  K.Cancel.cancel request_cancel)
              ()
          in
          Atomic.set start true;
          List.iter Thread.join workers;
          K.Cancel.cancel watchdog_stop;
          Thread.join watchdog;
          Array.iter
            (function
              | Some (Ok mapping) ->
                  Alcotest.(check string)
                    "concurrent resolved resource" "deployments"
                    mapping.K.Discovery.Mapper.api.plural
              | Some (Error error) ->
                  Alcotest.fail
                    ("concurrent discovery failed: "
                    ^ Format.asprintf "%a" K.Client.pp_error error)
              | None -> Alcotest.fail "concurrent discovery produced no result")
            results;
          Alcotest.(check (list string))
            "concurrent callers share one request" [ "/apis/apps/v1" ]
            (List.map request_target (Fake_api_server.requests server))))

let with_server replies fn =
  let listener = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> try Unix.close listener with Unix.Unix_error _ -> ())
    (fun () ->
      Unix.setsockopt listener Unix.SO_REUSEADDR true;
      Unix.bind listener (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
      Unix.listen listener (List.length replies);
      let port =
        match Unix.getsockname listener with
        | Unix.ADDR_INET (_, port) -> port
        | Unix.ADDR_UNIX _ -> assert false
      in
      let requests = ref [] in
      let server =
        Thread.create
          (fun () ->
            List.iter
              (fun reply ->
                let client, _ = Unix.accept listener in
                Fun.protect
                  ~finally:(fun () -> Unix.close client)
                  (fun () ->
                    requests := read_request client :: !requests;
                    write_all client reply))
              replies)
          ()
      in
      let config =
        K.Config.make
          (Uri.of_string (Printf.sprintf "http://127.0.0.1:%d" port))
      in
      let result = fn port config in
      Thread.join server;
      (result, List.rev !requests))

let with_stalled_tls_peer fn =
  let listener = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> try Unix.close listener with Unix.Unix_error _ -> ())
    (fun () ->
      Unix.setsockopt listener Unix.SO_REUSEADDR true;
      Unix.bind listener (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
      Unix.listen listener 1;
      let port =
        match Unix.getsockname listener with
        | Unix.ADDR_INET (_, port) -> port
        | Unix.ADDR_UNIX _ -> assert false
      in
      let accepted = Atomic.make false in
      let peer_stop = K.Cancel.create () in
      let server =
        Thread.create
          (fun () ->
            let client, _ = Unix.accept listener in
            Fun.protect
              ~finally:(fun () -> Unix.close client)
              (fun () ->
                Atomic.set accepted true;
                ignore (K.Cancel.sleep peer_stop 5.0)))
          ()
      in
      let config =
        K.Config.make
          ~tls:{ K.Config.default_tls with insecure_skip_verify = true }
          (Uri.of_string (Printf.sprintf "https://127.0.0.1:%d" port))
      in
      Fun.protect
        ~finally:(fun () ->
          K.Cancel.cancel peer_stop;
          Thread.join server)
        (fun () -> fn accepted config))

let test_connection_establishment_timeout () =
  with_stalled_tls_peer (fun _accepted config ->
      let started = Unix.gettimeofday () in
      let result =
        K.Http.request_once ~connect_timeout:0.2 config `GET "/version"
      in
      let elapsed = Unix.gettimeofday () -. started in
      (match result with
      | Error message ->
          Alcotest.(check bool)
            "classified connection timeout" true
            (contains_substring message "connection establishment timed out")
      | Ok _ -> Alcotest.fail "stalled TLS handshake unexpectedly completed");
      Alcotest.(check bool) "connection timeout is bounded" true (elapsed < 1.0))

let test_connection_establishment_cancellation () =
  with_stalled_tls_peer (fun accepted config ->
      let cancel = K.Cancel.create () in
      let result = ref None in
      let request =
        Thread.create
          (fun () ->
            result :=
              Some
                (K.Http.request_once ~cancel ~connect_timeout:5.0 config `GET
                   "/version"))
          ()
      in
      let deadline = Unix.gettimeofday () +. 1.0 in
      while (not (Atomic.get accepted)) && Unix.gettimeofday () < deadline do
        Unix.sleepf 0.001
      done;
      if not (Atomic.get accepted) then (
        K.Cancel.cancel cancel;
        Thread.join request;
        Alcotest.fail "TLS peer did not accept the test connection");
      let started = Unix.gettimeofday () in
      K.Cancel.cancel cancel;
      Thread.join request;
      let elapsed = Unix.gettimeofday () -. started in
      (match !result with
      | Some (Error message) ->
          Alcotest.(check bool)
            "classified connection cancellation" true
            (contains_substring message
               "request cancelled during connection establishment")
      | Some (Ok _) ->
          Alcotest.fail "cancelled TLS handshake unexpectedly completed"
      | None -> Alcotest.fail "cancelled request produced no result");
      Alcotest.(check bool)
        "connection cancellation is prompt" true (elapsed < 0.5))

let with_stalled_http_peer ~read_request_body fn =
  let listener = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> try Unix.close listener with Unix.Unix_error _ -> ())
    (fun () ->
      Unix.setsockopt listener Unix.SO_REUSEADDR true;
      Unix.bind listener (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
      Unix.listen listener 1;
      let port =
        match Unix.getsockname listener with
        | Unix.ADDR_INET (_, port) -> port
        | Unix.ADDR_UNIX _ -> assert false
      in
      let stop = K.Cancel.create () in
      let server =
        Thread.create
          (fun () ->
            let client, _ = Unix.accept listener in
            Fun.protect
              ~finally:(fun () -> Unix.close client)
              (fun () ->
                Unix.setsockopt_int client Unix.SO_RCVBUF 1024;
                if read_request_body then ignore (read_request client);
                ignore (K.Cancel.sleep stop 5.0)))
          ()
      in
      let config =
        K.Config.make
          (Uri.of_string (Printf.sprintf "http://127.0.0.1:%d" port))
      in
      Fun.protect
        ~finally:(fun () ->
          K.Cancel.cancel stop;
          Thread.join server)
        (fun () -> fn config))

let test_response_header_timeout () =
  with_stalled_http_peer ~read_request_body:true (fun config ->
      let started = Unix.gettimeofday () in
      let result =
        K.Http.request_once ~response_header_timeout:0.05 config `GET "/version"
      in
      let elapsed = Unix.gettimeofday () -. started in
      (match result with
      | Error message ->
          Alcotest.(check bool)
            "classified response header timeout" true
            (contains_substring message "response headers timed out after")
      | Ok _ -> Alcotest.fail "stalled response unexpectedly completed");
      Alcotest.(check bool) "response timeout is bounded" true (elapsed < 0.5))

let test_request_write_timeout () =
  with_stalled_http_peer ~read_request_body:false (fun config ->
      let body = String.make (16 * 1024 * 1024) 'x' in
      let started = Unix.gettimeofday () in
      let result =
        K.Http.request_once ~write_timeout:0.05 ~body config `POST "/upload"
      in
      let elapsed = Unix.gettimeofday () -. started in
      (match result with
      | Error message ->
          Alcotest.(check bool)
            "classified write timeout" true
            (contains_substring message "request write timed out after")
      | Ok _ -> Alcotest.fail "stalled request write unexpectedly completed");
      Alcotest.(check bool) "write timeout is bounded" true (elapsed < 1.0))

let test_http_forward_proxy () =
  let listener = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> try Unix.close listener with Unix.Unix_error _ -> ())
    (fun () ->
      Unix.setsockopt listener Unix.SO_REUSEADDR true;
      Unix.bind listener (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
      Unix.listen listener 1;
      let port =
        match Unix.getsockname listener with
        | Unix.ADDR_INET (_, port) -> port
        | Unix.ADDR_UNIX _ -> assert false
      in
      let captured = ref "" in
      let server =
        Thread.create
          (fun () ->
            let client, _ = Unix.accept listener in
            Fun.protect
              ~finally:(fun () -> Unix.close client)
              (fun () ->
                captured := read_request client;
                write_all client (response {|{"gitVersion":"v1.test"}|})))
          ()
      in
      let proxy_url =
        Uri.of_string
          (Printf.sprintf "http://proxy-user:proxy-password@127.0.0.1:%d" port)
      in
      let config =
        K.Config.make ~proxy_url
          (Uri.of_string "http://kubernetes.example.test:8080/prefix")
      in
      let result = K.Http.request_once config `GET "/version?verbose=true" in
      Thread.join server;
      (match result with
      | Error message -> Alcotest.fail message
      | Ok response -> Alcotest.(check int) "proxy response" 200 response.status);
      Alcotest.(check bool)
        "absolute proxy request target" true
        (String.starts_with
           ~prefix:
             "GET \
              http://kubernetes.example.test:8080/prefix/version?verbose=true \
              HTTP/1.1\r\n"
           !captured);
      Alcotest.(check bool)
        "API server Host header" true
        (contains_substring !captured
           "\r\nHost: kubernetes.example.test:8080\r\n");
      Alcotest.(check bool)
        "proxy basic authorization" true
        (contains_substring !captured
           "\r\n\
            Proxy-Authorization: Basic cHJveHktdXNlcjpwcm94eS1wYXNzd29yZA==\r\n"))

let test_http_connect_proxy_failure () =
  let listener = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> try Unix.close listener with Unix.Unix_error _ -> ())
    (fun () ->
      Unix.setsockopt listener Unix.SO_REUSEADDR true;
      Unix.bind listener (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
      Unix.listen listener 1;
      let port =
        match Unix.getsockname listener with
        | Unix.ADDR_INET (_, port) -> port
        | Unix.ADDR_UNIX _ -> assert false
      in
      let captured = ref "" in
      let server =
        Thread.create
          (fun () ->
            let client, _ = Unix.accept listener in
            Fun.protect
              ~finally:(fun () -> Unix.close client)
              (fun () ->
                captured := read_request client;
                write_all client
                  "HTTP/1.1 403 Forbidden\r\nContent-Length: 0\r\n\r\n"))
          ()
      in
      let config =
        K.Config.make
          ~proxy_url:
            (Uri.of_string
               (Printf.sprintf "http://proxy-user:proxy-password@127.0.0.1:%d"
                  port))
          (Uri.of_string "https://kubernetes.example.test:6443")
      in
      let result = K.Http.request_once config `GET "/version" in
      Thread.join server;
      (match result with
      | Error message ->
          Alcotest.(check bool)
            "CONNECT status is reported" true
            (contains_substring message "CONNECT failed with status 403")
      | Ok _ -> Alcotest.fail "rejected proxy tunnel unexpectedly succeeded");
      Alcotest.(check bool)
        "CONNECT authority" true
        (String.starts_with
           ~prefix:
             "CONNECT kubernetes.example.test:6443 HTTP/1.1\r\n\
              Host: kubernetes.example.test:6443\r\n"
           !captured);
      Alcotest.(check bool)
        "CONNECT proxy authorization" true
        (contains_substring !captured
           "\r\n\
            Proxy-Authorization: Basic cHJveHktdXNlcjpwcm94eS1wYXNzd29yZA==\r\n"))

let test_client_impersonation () =
  let impersonation =
    match
      K.Config.make_impersonation ~groups:[ "developers" ]
        ~extra:[ ("example.com/scope", [ "read" ]) ]
        ~user:"alice" ()
    with
    | Ok value -> value
    | Error message -> Alcotest.fail message
  in
  let result, requests =
    with_server
      [ response "{}"; response "{}" ]
      (fun _port base_config ->
        let config = K.Config.make ~impersonation base_config.server in
        let client = K.Client.create config in
        Fun.protect
          ~finally:(fun () -> K.Client.close client)
          (fun () ->
            let first = K.Client.raw client `GET "/configured" in
            let second =
              K.Client.raw
                ~headers:
                  [
                    ("Impersonate-User", "bob");
                    ("Impersonate-Group", "operators");
                  ]
                client `GET "/overridden"
            in
            (first, second)))
  in
  let check_ok = function
    | Ok _ -> ()
    | Error error ->
        Alcotest.fail (Format.asprintf "%a" K.Client.pp_error error)
  in
  let first, second = result in
  check_ok first;
  check_ok second;
  match requests with
  | [ configured; overridden ] ->
      Alcotest.(check bool)
        "configured user" true
        (contains_substring configured "\r\nImpersonate-User: alice\r\n");
      Alcotest.(check bool)
        "configured group" true
        (contains_substring configured "\r\nImpersonate-Group: developers\r\n");
      Alcotest.(check bool)
        "configured extra" true
        (contains_substring configured
           "\r\nImpersonate-Extra-example.com%2Fscope: read\r\n");
      Alcotest.(check bool)
        "explicit user overrides configured identity" true
        (contains_substring overridden "\r\nImpersonate-User: bob\r\n"
        && contains_substring overridden "\r\nImpersonate-Group: operators\r\n"
        && (not (contains_substring overridden "alice"))
        && not (contains_substring overridden "developers"))
  | _ -> Alcotest.fail "expected two impersonated requests"

let test_socks5_proxy () =
  let read_exact fd length =
    let value = Bytes.create length in
    let rec loop offset =
      if offset < length then
        let count = Unix.read fd value offset (length - offset) in
        if count = 0 then Alcotest.fail "SOCKS client closed during handshake"
        else loop (offset + count)
    in
    loop 0;
    Bytes.unsafe_to_string value
  in
  let listener = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> try Unix.close listener with Unix.Unix_error _ -> ())
    (fun () ->
      Unix.setsockopt listener Unix.SO_REUSEADDR true;
      Unix.bind listener (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
      Unix.listen listener 1;
      let proxy_port =
        match Unix.getsockname listener with
        | Unix.ADDR_INET (_, port) -> port
        | Unix.ADDR_UNIX _ -> assert false
      in
      let destination = ref None in
      let captured = ref "" in
      let server =
        Thread.create
          (fun () ->
            let client, _ = Unix.accept listener in
            Fun.protect
              ~finally:(fun () -> Unix.close client)
              (fun () ->
                Alcotest.(check string)
                  "SOCKS greeting" "\005\002\000\002" (read_exact client 4);
                write_all client "\005\002";
                let auth_head = read_exact client 2 in
                Alcotest.(check int)
                  "SOCKS auth version" 1
                  (Char.code auth_head.[0]);
                let user = read_exact client (Char.code auth_head.[1]) in
                let password_length =
                  read_exact client 1 |> fun value -> Char.code value.[0]
                in
                let password = read_exact client password_length in
                Alcotest.(check string) "SOCKS username" "proxy-user" user;
                Alcotest.(check string)
                  "SOCKS password" "proxy-password" password;
                write_all client "\001\000";
                let head = read_exact client 5 in
                Alcotest.(check int) "SOCKS version" 5 (Char.code head.[0]);
                Alcotest.(check int)
                  "SOCKS connect command" 1
                  (Char.code head.[1]);
                Alcotest.(check int)
                  "SOCKS domain address" 3
                  (Char.code head.[3]);
                let host = read_exact client (Char.code head.[4]) in
                let port_bytes = read_exact client 2 in
                let port =
                  (Char.code port_bytes.[0] lsl 8) lor Char.code port_bytes.[1]
                in
                destination := Some (host, port);
                write_all client "\005\000\000\001\127\000\000\001\000\000";
                captured := read_request client;
                write_all client (response "{}")))
          ()
      in
      let config =
        K.Config.make
          ~proxy_url:
            (Uri.of_string
               (Printf.sprintf "socks5://proxy-user:proxy-password@127.0.0.1:%d"
                  proxy_port))
          (Uri.of_string "http://kubernetes.example.test:8080")
      in
      let result = K.Http.request_once config `GET "/version" in
      Thread.join server;
      (match result with
      | Error message -> Alcotest.fail message
      | Ok response -> Alcotest.(check int) "SOCKS response" 200 response.status);
      Alcotest.(check (option (pair string int)))
        "SOCKS destination"
        (Some ("kubernetes.example.test", 8080))
        !destination;
      Alcotest.(check bool)
        "origin-form request after SOCKS tunnel" true
        (String.starts_with ~prefix:"GET /version HTTP/1.1\r\n" !captured))

let test_streaming_http () =
  let listener = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> try Unix.close listener with Unix.Unix_error _ -> ())
    (fun () ->
      Unix.setsockopt listener Unix.SO_REUSEADDR true;
      Unix.bind listener (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
      Unix.listen listener 1;
      let port =
        match Unix.getsockname listener with
        | Unix.ADDR_INET (_, port) -> port
        | Unix.ADDR_UNIX _ -> assert false
      in
      let server =
        Thread.create
          (fun () ->
            let client, _ = Unix.accept listener in
            Fun.protect
              ~finally:(fun () -> Unix.close client)
              (fun () ->
                let buffer = Bytes.create 4096 in
                ignore (Unix.read client buffer 0 (Bytes.length buffer));
                write_all client
                  "HTTP/1.1 200 OK\r\n\
                   Transfer-Encoding: chunked\r\n\
                   \r\n\
                   5\r\n\
                   hello\r\n\
                   6\r\n\
                  \ world\r\n\
                   0\r\n\
                   \r\n"))
          ()
      in
      let config =
        K.Config.make
          (Uri.of_string (Printf.sprintf "http://127.0.0.1:%d" port))
      in
      let streamed = Buffer.create 16 in
      let response =
        K.Http.request_once
          ~on_chunk:(Buffer.add_string streamed)
          config `GET "/stream"
      in
      Thread.join server;
      match response with
      | Error message -> Alcotest.fail message
      | Ok response ->
          Alcotest.(check int) "status" 200 response.status;
          Alcotest.(check string)
            "streamed bytes" "hello world" (Buffer.contents streamed);
          Alcotest.(check string) "stream does not buffer body" "" response.body)

let test_http_connection_reuse () =
  let listener = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> try Unix.close listener with Unix.Unix_error _ -> ())
    (fun () ->
      Unix.setsockopt listener Unix.SO_REUSEADDR true;
      Unix.bind listener (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
      Unix.listen listener 3;
      let port =
        match Unix.getsockname listener with
        | Unix.ADDR_INET (_, port) -> port
        | Unix.ADDR_UNIX _ -> assert false
      in
      let accepted = Atomic.make 0 in
      let requests = ref [] in
      let reply ?(connection = "keep-alive") body =
        Printf.sprintf
          "HTTP/1.1 200 OK\r\nContent-Length: %d\r\nConnection: %s\r\n\r\n%s"
          (String.length body) connection body
      in
      let server =
        Thread.create
          (fun () ->
            let first, _ = Unix.accept listener in
            Atomic.incr accepted;
            Fun.protect
              ~finally:(fun () -> Unix.close first)
              (fun () ->
                requests := read_request first :: !requests;
                write_all first (reply "first");
                let readable, _, _ = Unix.select [ first; listener ] [] [] 5. in
                let second =
                  if List.mem first readable then first
                  else if List.mem listener readable then (
                    let connection, _ = Unix.accept listener in
                    Atomic.incr accepted;
                    connection)
                  else (
                    Unix.shutdown first Unix.SHUTDOWN_ALL;
                    raise (Failure "timed out waiting for the second request"))
                in
                Fun.protect
                  ~finally:(fun () -> if second <> first then Unix.close second)
                  (fun () ->
                    requests := read_request second :: !requests;
                    write_all second (reply ~connection:"close" "second"))))
          ()
      in
      let config =
        K.Config.make
          (Uri.of_string (Printf.sprintf "http://127.0.0.1:%d" port))
      in
      let client = K.Client.create config in
      let result =
        Fun.protect
          ~finally:(fun () -> K.Client.close client)
          (fun () ->
            let first = K.Client.raw client `GET "/first" in
            let second = K.Client.raw client `GET "/second" in
            (first, second))
      in
      let after_close = K.Client.raw client `GET "/after-close" in
      Thread.join server;
      let check_body expected = function
        | Error error ->
            Alcotest.fail (Format.asprintf "%a" K.Client.pp_error error)
        | Ok (response : K.Http.response) ->
            Alcotest.(check string) "response body" expected response.body
      in
      let first, second = result in
      check_body "first" first;
      check_body "second" second;
      Alcotest.(check int) "one TCP connection" 1 (Atomic.get accepted);
      Alcotest.(check int) "two requests" 2 (List.length !requests);
      match after_close with
      | Error (K.Client.Transport "HTTP transport is closed") -> ()
      | Error error ->
          Alcotest.fail
            ("unexpected post-close error: "
            ^ Format.asprintf "%a" K.Client.pp_error error)
      | Ok _ -> Alcotest.fail "closed client accepted a new request")

let test_shared_reflector_cache () =
  let listener = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> try Unix.close listener with Unix.Unix_error _ -> ())
    (fun () ->
      Unix.setsockopt listener Unix.SO_REUSEADDR true;
      Unix.bind listener (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
      Unix.listen listener 2;
      let port =
        match Unix.getsockname listener with
        | Unix.ADDR_INET (_, port) -> port
        | Unix.ADDR_UNIX _ -> assert false
      in
      let cancel = K.Cancel.create () in
      let watch_started = Atomic.make false in
      let server_error = Atomic.make None in
      let requests = ref [] in
      let server =
        Thread.create
          (fun () ->
            try
              let failed, _ = Unix.accept listener in
              Fun.protect
                ~finally:(fun () -> Unix.close failed)
                (fun () ->
                  requests := read_request failed :: !requests;
                  write_all failed
                    (response ~status:"500 Internal Server Error"
                       {|{"kind":"Status","apiVersion":"v1","status":"Failure","message":"temporary list failure","reason":"InternalError","code":500}|}));
              let first, _ = Unix.accept listener in
              Fun.protect
                ~finally:(fun () -> Unix.close first)
                (fun () ->
                  requests := read_request first :: !requests;
                  let snapshot =
                    list_json ~resource_version:"40"
                      [ widget_json ~name:"shared" ~resource_version:"40" () ]
                    |> Yojson.Safe.to_string |> response
                  in
                  write_all first snapshot);
              let readable, _, _ = Unix.select [ listener ] [] [] 5. in
              if readable = [] then
                raise (Failure "timed out waiting for the shared watch");
              let watched, _ = Unix.accept listener in
              Fun.protect
                ~finally:(fun () -> Unix.close watched)
                (fun () ->
                  requests := read_request watched :: !requests;
                  Atomic.set watch_started true;
                  (try
                     write_all watched
                       "HTTP/1.1 200 OK\r\n\
                        Transfer-Encoding: chunked\r\n\
                        Connection: close\r\n\
                        \r\n"
                   with Unix.Unix_error _ -> ());
                  let buffer = Bytes.create 64 in
                  try
                    while
                      Unix.read watched buffer 0 (Bytes.length buffer) > 0
                    do
                      ()
                    done
                  with Unix.Unix_error _ -> ())
            with exn ->
              Atomic.set server_error (Some (Printexc.to_string exn));
              Atomic.set watch_started true;
              K.Cancel.cancel cancel)
          ()
      in
      let config =
        K.Config.make
          (Uri.of_string (Printf.sprintf "http://127.0.0.1:%d" port))
      in
      let client = K.Client.create config in
      Fun.protect
        ~finally:(fun () -> K.Client.close client)
        (fun () ->
          let cache = Widget_controller.Cache.create () in
          (match
             Widget_controller.Cache.add_index cache ~name:"metadata.name"
               (fun value -> [ value.K.Dynamic.metadata.name ])
           with
          | Ok () -> ()
          | Error message -> Alcotest.fail message);
          let metrics = K.Metrics.create () in
          let first_reconciles = Atomic.make 0 in
          let second_reconciles = Atomic.make 0 in
          let cancellation_propagated = Atomic.make true in
          let cached_reads = Atomic.make 0 in
          let cached_misses = Atomic.make 0 in
          let cached_lists = Atomic.make 0 in
          let cached_indexes = Atomic.make 0 in
          let reconcile counter _client request =
            if request.Widget_controller.cancel != cancel then
              Atomic.set cancellation_propagated false;
            let rec await_watch () =
              if
                (not (Atomic.get watch_started))
                && not (K.Cancel.is_cancelled cancel)
              then (
                Unix.sleepf 0.001;
                await_watch ())
            in
            await_watch ();
            (match
               Widget_controller.Cached.get request.Widget_controller.reader
                 ~namespace:"default" "shared"
             with
            | Ok value when value.K.Dynamic.metadata.name = "shared" ->
                Atomic.incr cached_reads
            | Ok _ -> Alcotest.fail "cached GET returned the wrong object"
            | Error error ->
                Alcotest.fail
                  ("cached GET failed: "
                  ^ Format.asprintf "%a" K.Client.pp_error error));
            (match
               Widget_controller.Cached.get request.Widget_controller.reader
                 ~namespace:"default" "missing"
             with
            | Error error when K.Client.Error.is_not_found error ->
                Atomic.incr cached_misses
            | Error error ->
                Alcotest.fail
                  ("cached miss returned the wrong error: "
                  ^ Format.asprintf "%a" K.Client.pp_error error)
            | Ok _ -> Alcotest.fail "cached miss returned an object");
            (match
               Widget_controller.Cached.list ~namespace:"default"
                 request.Widget_controller.reader
             with
            | Ok [ value ] when value.K.Dynamic.metadata.name = "shared" ->
                Atomic.incr cached_lists
            | Ok values ->
                Alcotest.fail
                  (Printf.sprintf "cached LIST returned %d objects"
                     (List.length values))
            | Error error ->
                Alcotest.fail
                  ("cached LIST failed: "
                  ^ Format.asprintf "%a" K.Client.pp_error error));
            (match
               Widget_controller.Cached.by_index
                 request.Widget_controller.reader ~name:"metadata.name" "shared"
             with
            | Ok [ value ] when value.K.Dynamic.metadata.name = "shared" ->
                Atomic.incr cached_indexes
            | Ok values ->
                Alcotest.fail
                  (Printf.sprintf "cached index returned %d objects"
                     (List.length values))
            | Error error ->
                Alcotest.fail
                  ("cached index failed: "
                  ^ Format.asprintf "%a" K.Client.pp_error error));
            (match request.Widget_controller.resource with
            | Some _ -> Atomic.incr counter
            | None -> ());
            if
              Atomic.get first_reconciles > 0
              && Atomic.get second_reconciles > 0
            then K.Cancel.cancel cancel;
            Ok K.Controller.Done
          in
          let manager = K.Manager.create ~cancel client in
          K.Manager.add manager
            (Widget_controller.component ~cache ~name:"shared-first" ~metrics
               ~reconcile:(reconcile first_reconciles)
               ());
          K.Manager.add manager
            (Widget_controller.component ~cache ~name:"shared-second" ~metrics
               ~reconcile:(reconcile second_reconciles)
               ());
          (match K.Manager.run manager with
          | Ok () -> ()
          | Error error ->
              Alcotest.fail (Format.asprintf "%a" K.Manager.pp_error error));
          Thread.join server;
          Option.iter Alcotest.fail (Atomic.get server_error);
          Alcotest.(check int)
            "first controller reconciled" 1
            (Atomic.get first_reconciles);
          Alcotest.(check int)
            "second controller reconciled" 1
            (Atomic.get second_reconciles);
          Alcotest.(check bool)
            "controller cancellation reaches reconciler" true
            (Atomic.get cancellation_propagated);
          Alcotest.(check int) "two cached GETs" 2 (Atomic.get cached_reads);
          Alcotest.(check int)
            "two classified cached misses" 2 (Atomic.get cached_misses);
          Alcotest.(check int) "two cached LISTs" 2 (Atomic.get cached_lists);
          Alcotest.(check int)
            "two cached index reads" 2
            (Atomic.get cached_indexes);
          let rendered_metrics = K.Metrics.render metrics in
          List.iter
            (fun controller ->
              Alcotest.(check bool)
                (controller ^ " reconciliation metric")
                true
                (contains_substring rendered_metrics
                   ("ocaml_k8s_reconciliations_total{controller=\"" ^ controller
                  ^ "\",result=\"done\"} 1"));
              Alcotest.(check bool)
                (controller ^ " active workers drained")
                true
                (contains_substring rendered_metrics
                   ("ocaml_k8s_active_reconcile_workers{controller=\""
                  ^ controller ^ "\"} 0")))
            [ "shared-first"; "shared-second" ];
          let targets = List.rev_map request_target !requests in
          let lists, watches =
            List.partition
              (fun target ->
                Uri.get_query_param (Uri.of_string target) "watch" = None)
              targets
          in
          match (lists, watches) with
          | [ first_list; second_list ], [ watched ] ->
              List.iter
                (fun listed ->
                  Alcotest.(check (option string))
                    "LIST has no watch flag" None
                    (Uri.get_query_param (Uri.of_string listed) "watch"))
                [ first_list; second_list ];
              Alcotest.(check (option string))
                "one shared WATCH" (Some "true")
                (Uri.get_query_param (Uri.of_string watched) "watch")
          | _ ->
              Alcotest.fail
                (Printf.sprintf
                   "expected two LIST attempts and one WATCH, got %d requests"
                   (List.length targets))))

let test_owned_resource_watch () =
  let listener = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> try Unix.close listener with Unix.Unix_error _ -> ())
    (fun () ->
      Unix.setsockopt listener Unix.SO_REUSEADDR true;
      Unix.bind listener (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
      Unix.listen listener 8;
      let port =
        match Unix.getsockname listener with
        | Unix.ADDR_INET (_, port) -> port
        | Unix.ADDR_UNIX _ -> assert false
      in
      let cancel = K.Cancel.create () in
      let watch_count = Atomic.make 0 in
      let initial_owner_reconciled = Atomic.make false in
      let previous_owner_reconciled = Atomic.make false in
      let current_owner_reconciled = Atomic.make false in
      let server_error = Atomic.make None in
      let request_lock = Mutex.create () in
      let requests = ref [] in
      let record request =
        Mutex.lock request_lock;
        requests := request :: !requests;
        Mutex.unlock request_lock
      in
      let gadget ~owner ~resource_version =
        `Assoc
          [
            ("apiVersion", `String "testing.ocaml-k8s.dev/v1");
            ("kind", `String "Gadget");
            ( "metadata",
              `Assoc
                [
                  ("name", `String "dependent");
                  ("namespace", `String "default");
                  ("resourceVersion", `String resource_version);
                  ( "ownerReferences",
                    `List
                      [
                        `Assoc
                          [
                            ("apiVersion", `String "testing.ocaml-k8s.dev/v1");
                            ("kind", `String "Widget");
                            ("name", `String owner);
                            ("uid", `String (owner ^ "-uid"));
                            ("controller", `Bool true);
                            ("blockOwnerDeletion", `Bool true);
                          ];
                      ] );
                ] );
            ("spec", `Assoc []);
          ]
      in
      let list_body items =
        list_json ~resource_version:"51" items
        |> Yojson.Safe.to_string |> response
      in
      let handle connection =
        Fun.protect
          ~finally:(fun () -> Unix.close connection)
          (fun () ->
            let request = read_request connection in
            record request;
            let target = request_target request in
            let uri = Uri.of_string target in
            let path = Uri.path uri in
            let is_gadget = String.ends_with ~suffix:"/gadgets" path in
            match Uri.get_query_param uri "watch" with
            | Some "true" -> (
                Atomic.incr watch_count;
                (try
                   write_all connection
                     "HTTP/1.1 200 OK\r\n\
                      Transfer-Encoding: chunked\r\n\
                      Connection: close\r\n\
                      \r\n"
                 with Unix.Unix_error _ -> ());
                if is_gadget then (
                  while
                    (not (Atomic.get initial_owner_reconciled))
                    && not (K.Cancel.is_cancelled cancel)
                  do
                    Unix.sleepf 0.001
                  done;
                  if not (K.Cancel.is_cancelled cancel) then
                    let event =
                      `Assoc
                        [
                          ("type", `String "MODIFIED");
                          ( "object",
                            gadget ~owner:"new-owner" ~resource_version:"52" );
                        ]
                      |> Yojson.Safe.to_string
                      |> fun value -> value ^ "\n"
                    in
                    let chunk =
                      Printf.sprintf "%x\r\n%s\r\n" (String.length event) event
                    in
                    try write_all connection chunk
                    with Unix.Unix_error _ -> ());
                let buffer = Bytes.create 64 in
                try
                  while
                    Unix.read connection buffer 0 (Bytes.length buffer) > 0
                  do
                    ()
                  done
                with Unix.Unix_error _ -> ())
            | _ ->
                write_all connection
                  (list_body
                     (if is_gadget then
                        [ gadget ~owner:"owner" ~resource_version:"51" ]
                      else [])))
      in
      let server =
        Thread.create
          (fun () ->
            let handlers = ref [] in
            let deadline = Unix.gettimeofday () +. 5.0 in
            (try
               while not (K.Cancel.is_cancelled cancel) do
                 if Unix.gettimeofday () >= deadline then
                   raise
                     (Failure "timed out waiting for owned-resource reconcile");
                 let readable, _, _ = Unix.select [ listener ] [] [] 0.05 in
                 if readable <> [] then
                   let connection, _ = Unix.accept listener in
                   handlers := Thread.create handle connection :: !handlers
               done
             with exn ->
               Atomic.set server_error (Some (Printexc.to_string exn));
               K.Cancel.cancel cancel);
            List.iter Thread.join !handlers)
          ()
      in
      let config =
        K.Config.make
          (Uri.of_string (Printf.sprintf "http://127.0.0.1:%d" port))
      in
      let client = K.Client.create config in
      Fun.protect
        ~finally:(fun () -> K.Client.close client)
        (fun () ->
          let reconcile _client request =
            let rec await_watches () =
              if
                Atomic.get watch_count < 2 && not (K.Cancel.is_cancelled cancel)
              then (
                Unix.sleepf 0.001;
                await_watches ())
            in
            await_watches ();
            (if Atomic.compare_and_set initial_owner_reconciled false true then
               ()
             else
               let previous =
                 K.Core.Object_key.make ~namespace:"default" "owner"
               in
               let current =
                 K.Core.Object_key.make ~namespace:"default" "new-owner"
               in
               if K.Core.Object_key.equal request.Widget_controller.key previous
               then Atomic.set previous_owner_reconciled true;
               if K.Core.Object_key.equal request.Widget_controller.key current
               then Atomic.set current_owner_reconciled true;
               if
                 Atomic.get previous_owner_reconciled
                 && Atomic.get current_owner_reconciled
               then K.Cancel.cancel cancel);
            Ok K.Controller.Done
          in
          let manager = K.Manager.create ~cancel client in
          let owned =
            Widget_owns_gadget.make ~field_selector:"metadata.namespace=default"
              ()
          in
          K.Manager.add manager
            (Widget_controller.component ~name:"owned-watch"
               ~predicate:(fun _ -> false)
               ~watches:[ owned ] ~reconcile ());
          (match K.Manager.run manager with
          | Ok () -> ()
          | Error error ->
              Alcotest.fail (Format.asprintf "%a" K.Manager.pp_error error));
          Thread.join server;
          Option.iter Alcotest.fail (Atomic.get server_error);
          Alcotest.(check bool)
            "ownership transfer reconciles previous owner" true
            (Atomic.get previous_owner_reconciled);
          Alcotest.(check bool)
            "ownership transfer reconciles current owner" true
            (Atomic.get current_owner_reconciled);
          Mutex.lock request_lock;
          let targets = List.rev_map request_target !requests in
          Mutex.unlock request_lock;
          let lists, watches =
            List.partition
              (fun target ->
                Uri.get_query_param (Uri.of_string target) "watch" = None)
              targets
          in
          Alcotest.(check int) "two synchronized caches" 2 (List.length lists);
          Alcotest.(check int) "two watch streams" 2 (List.length watches);
          let gadget_targets =
            List.filter
              (fun target ->
                String.starts_with
                  ~prefix:"/apis/testing.ocaml-k8s.dev/v1/gadgets" target)
              targets
          in
          Alcotest.(check int)
            "dependent list and watch selected" 2
            (List.length gadget_targets);
          List.iter
            (fun target ->
              Alcotest.(check (option string))
                "field selector propagated" (Some "metadata.namespace=default")
                (Uri.get_query_param (Uri.of_string target) "fieldSelector"))
            gadget_targets))

let test_leader_election_lifecycle () =
  let listener = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> try Unix.close listener with Unix.Unix_error _ -> ())
    (fun () ->
      Unix.setsockopt listener Unix.SO_REUSEADDR true;
      Unix.bind listener (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
      Unix.listen listener 8;
      let port =
        match Unix.getsockname listener with
        | Unix.ADDR_INET (_, port) -> port
        | Unix.ADDR_UNIX _ -> assert false
      in
      let outer_cancel = K.Cancel.create () in
      let stop_server = Atomic.make false in
      let server_error = Atomic.make None in
      let creates = Atomic.make 0 in
      let renewals = Atomic.make 0 in
      let releases = Atomic.make 0 in
      let resource_version = ref 0 in
      let lease = ref None in
      let json_member name = function
        | `Assoc fields -> List.assoc_opt name fields
        | _ -> None
      in
      let json_string = function
        | `String value -> Some value
        | _ -> None
      in
      let holder json =
        let value =
          Option.bind (json_member "spec" json) (json_member "holderIdentity")
        in
        Option.bind value json_string
      in
      let validate_microtimes json =
        let spec = json_member "spec" json in
        List.iter
          (fun name ->
            match
              Option.bind spec (json_member name) |> fun value ->
              Option.bind value json_string
            with
            | None -> raise (Failure (name ^ " is missing"))
            | Some value -> (
                match String.rindex_opt value '.' with
                | Some point
                  when String.ends_with ~suffix:"Z" value
                       && String.length value - point - 2 = 6 -> ()
                | _ ->
                    raise
                      (Failure
                         (name ^ " is not a six-digit Kubernetes MicroTime"))))
          [ "acquireTime"; "renewTime" ]
      in
      let assign_resource_version json =
        incr resource_version;
        match json with
        | `Assoc fields ->
            let metadata =
              match List.assoc_opt "metadata" fields with
              | Some (`Assoc fields) -> fields
              | _ -> []
            in
            let metadata =
              ("resourceVersion", `String (string_of_int !resource_version))
              :: List.remove_assoc "resourceVersion" metadata
            in
            `Assoc
              (("metadata", `Assoc metadata)
              :: List.remove_assoc "metadata" fields)
        | value -> value
      in
      let status_response status code message =
        `Assoc
          [
            ("apiVersion", `String "v1");
            ("kind", `String "Status");
            ("status", `String "Failure");
            ("message", `String message);
            ("code", `Int code);
          ]
        |> Yojson.Safe.to_string |> response ~status
      in
      let server =
        Thread.create
          (fun () ->
            try
              while not (Atomic.get stop_server) do
                let readable, _, _ = Unix.select [ listener ] [] [] 0.05 in
                if readable <> [] then
                  let connection, _ = Unix.accept listener in
                  Fun.protect
                    ~finally:(fun () -> Unix.close connection)
                    (fun () ->
                      let request = read_request connection in
                      let meth = request_method request in
                      match (meth, !lease) with
                      | "GET", None ->
                          write_all connection
                            (status_response "404 Not Found" 404
                               "leader Lease not found")
                      | "GET", Some value ->
                          write_all connection
                            (response (Yojson.Safe.to_string value))
                      | "POST", _ ->
                          let proposed =
                            request_body request |> Yojson.Safe.from_string
                          in
                          validate_microtimes proposed;
                          let value = assign_resource_version proposed in
                          lease := Some value;
                          Atomic.incr creates;
                          write_all connection
                            (response (Yojson.Safe.to_string value))
                      | "PUT", Some _ ->
                          let proposed =
                            request_body request |> Yojson.Safe.from_string
                          in
                          validate_microtimes proposed;
                          let value = assign_resource_version proposed in
                          lease := Some value;
                          let released = holder value = Some "" in
                          if released then Atomic.incr releases
                          else Atomic.incr renewals;
                          write_all connection
                            (response (Yojson.Safe.to_string value));
                          if not released then K.Cancel.cancel outer_cancel
                      | "PUT", None ->
                          write_all connection
                            (status_response "404 Not Found" 404
                               "leader Lease not found")
                      | _ ->
                          write_all connection
                            (status_response "405 Method Not Allowed" 405
                               "unexpected Lease request"))
              done
            with exn ->
              Atomic.set server_error (Some (Printexc.to_string exn));
              K.Cancel.cancel outer_cancel)
          ()
      in
      let config =
        K.Config.make
          (Uri.of_string (Printf.sprintf "http://127.0.0.1:%d" port))
      in
      let client = K.Client.create config in
      let phases = ref [] in
      let callback_stopped = Atomic.make false in
      let election =
        {
          (K.Leader_election.default ~namespace:"default" ~name:"test-lock"
             ~identity:"candidate-a")
          with
          lease_duration = 1.;
          renew_deadline = 0.6;
          retry_period = 0.2;
        }
      in
      let result =
        Fun.protect
          ~finally:(fun () ->
            Atomic.set stop_server true;
            K.Client.close client;
            Thread.join server)
          (fun () ->
            K.Leader_election.run ~cancel:outer_cancel
              ~on_phase:(fun phase -> phases := phase :: !phases)
              client election
              (fun leadership_cancel ->
                while not (K.Cancel.is_cancelled leadership_cancel) do
                  ignore (K.Cancel.sleep leadership_cancel 0.01)
                done;
                Atomic.set callback_stopped true;
                "leader work stopped"))
      in
      Option.iter Alcotest.fail (Atomic.get server_error);
      (match result with
      | Ok (K.Leader_election.Finished value) ->
          Alcotest.(check string) "callback result" "leader work stopped" value
      | Ok K.Leader_election.Cancelled_before_leadership ->
          Alcotest.fail "candidate never acquired the empty Lease"
      | Error error ->
          Alcotest.fail (Format.asprintf "%a" K.Leader_election.pp_error error));
      Alcotest.(check int) "Lease created once" 1 (Atomic.get creates);
      Alcotest.(check bool) "Lease renewed" true (Atomic.get renewals >= 1);
      Alcotest.(check int) "Lease released once" 1 (Atomic.get releases);
      Alcotest.(check bool)
        "protected callback joined" true
        (Atomic.get callback_stopped);
      Alcotest.(check bool)
        "phase lifecycle" true
        (!phases
        = [
            K.Leader_election.Stopped;
            K.Leader_election.Leading;
            K.Leader_election.Waiting;
          ]))

let test_leader_election_contention () =
  let listener = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> try Unix.close listener with Unix.Unix_error _ -> ())
    (fun () ->
      Unix.setsockopt listener Unix.SO_REUSEADDR true;
      Unix.bind listener (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
      Unix.listen listener 32;
      let port =
        match Unix.getsockname listener with
        | Unix.ADDR_INET (_, port) -> port
        | Unix.ADDR_UNIX _ -> assert false
      in
      let stop_server = Atomic.make false in
      let server_error = Atomic.make None in
      let cancel_a = K.Cancel.create () in
      let cancel_b = K.Cancel.create () in
      let resource_version = ref 0 in
      let lease = ref None in
      let json_member name = function
        | `Assoc fields -> List.assoc_opt name fields
        | _ -> None
      in
      let json_string = function
        | `String value -> Some value
        | _ -> None
      in
      let resource_version_of json =
        let metadata = json_member "metadata" json in
        Option.bind metadata (json_member "resourceVersion") |> fun value ->
        Option.bind value json_string
      in
      let assign_resource_version json =
        incr resource_version;
        match json with
        | `Assoc fields ->
            let metadata =
              match List.assoc_opt "metadata" fields with
              | Some (`Assoc fields) -> fields
              | _ -> []
            in
            let metadata =
              ("resourceVersion", `String (string_of_int !resource_version))
              :: List.remove_assoc "resourceVersion" metadata
            in
            `Assoc
              (("metadata", `Assoc metadata)
              :: List.remove_assoc "metadata" fields)
        | value -> value
      in
      let status_response ?reason status code message =
        `Assoc
          ([
             ("apiVersion", `String "v1");
             ("kind", `String "Status");
             ("status", `String "Failure");
             ("message", `String message);
             ("code", `Int code);
           ]
          @
          match reason with
          | None -> []
          | Some reason -> [ ("reason", `String reason) ])
        |> Yojson.Safe.to_string |> response ~status
      in
      let conflict connection =
        write_all connection
          (status_response ~reason:"Conflict" "409 Conflict" 409
             "Lease resourceVersion conflict")
      in
      let already_exists connection =
        write_all connection
          (status_response ~reason:"AlreadyExists" "409 Conflict" 409
             "Lease already exists")
      in
      let server =
        Thread.create
          (fun () ->
            try
              while not (Atomic.get stop_server) do
                let readable, _, _ = Unix.select [ listener ] [] [] 0.05 in
                if readable <> [] then
                  let connection, _ = Unix.accept listener in
                  Fun.protect
                    ~finally:(fun () -> Unix.close connection)
                    (fun () ->
                      let request = read_request connection in
                      match (request_method request, !lease) with
                      | "GET", None ->
                          write_all connection
                            (status_response "404 Not Found" 404
                               "leader Lease not found")
                      | "GET", Some value ->
                          write_all connection
                            (response (Yojson.Safe.to_string value))
                      | "POST", None ->
                          let value =
                            request_body request |> Yojson.Safe.from_string
                            |> assign_resource_version
                          in
                          lease := Some value;
                          write_all connection
                            (response (Yojson.Safe.to_string value))
                      | "POST", Some _ -> already_exists connection
                      | "PUT", Some current ->
                          let proposed =
                            request_body request |> Yojson.Safe.from_string
                          in
                          if
                            resource_version_of proposed
                            <> resource_version_of current
                          then conflict connection
                          else
                            let value = assign_resource_version proposed in
                            lease := Some value;
                            write_all connection
                              (response (Yojson.Safe.to_string value))
                      | "PUT", None ->
                          write_all connection
                            (status_response "404 Not Found" 404
                               "leader Lease not found")
                      | _ ->
                          write_all connection
                            (status_response "405 Method Not Allowed" 405
                               "unexpected Lease request"))
              done
            with exn ->
              Atomic.set server_error (Some (Printexc.to_string exn));
              K.Cancel.cancel cancel_a;
              K.Cancel.cancel cancel_b)
          ()
      in
      let client_config =
        K.Config.make
          (Uri.of_string (Printf.sprintf "http://127.0.0.1:%d" port))
      in
      let client_a = K.Client.create client_config in
      let client_b = K.Client.create client_config in
      let active = Atomic.make 0 in
      let maximum_active = Atomic.make 0 in
      let order_lock = Mutex.create () in
      let leader_order = ref [] in
      let update_maximum value =
        let rec loop () =
          let current = Atomic.get maximum_active in
          if
            value > current
            && not (Atomic.compare_and_set maximum_active current value)
          then loop ()
        in
        loop ()
      in
      let callback identity own_cancel leadership_cancel =
        let count = Atomic.fetch_and_add active 1 + 1 in
        update_maximum count;
        Mutex.lock order_lock;
        let first = !leader_order = [] in
        leader_order := identity :: !leader_order;
        Mutex.unlock order_lock;
        if first then (
          let timer =
            Thread.create
              (fun () ->
                Unix.sleepf 0.3;
                K.Cancel.cancel own_cancel)
              ()
          in
          while not (K.Cancel.is_cancelled leadership_cancel) do
            ignore (K.Cancel.sleep leadership_cancel 0.01)
          done;
          Thread.join timer)
        else K.Cancel.cancel own_cancel;
        Atomic.decr active;
        identity
      in
      let election identity =
        {
          (K.Leader_election.default ~namespace:"default" ~name:"contended-lock"
             ~identity)
          with
          lease_duration = 1.;
          renew_deadline = 0.6;
          retry_period = 0.2;
        }
      in
      let result_a = ref None in
      let result_b = ref None in
      let thread_a =
        Thread.create
          (fun () ->
            result_a :=
              Some
                (K.Leader_election.run ~cancel:cancel_a client_a
                   (election "candidate-a")
                   (callback "candidate-a" cancel_a)))
          ()
      in
      let thread_b =
        Thread.create
          (fun () ->
            result_b :=
              Some
                (K.Leader_election.run ~cancel:cancel_b client_b
                   (election "candidate-b")
                   (callback "candidate-b" cancel_b)))
          ()
      in
      let watchdog_stop = K.Cancel.create () in
      let watchdog =
        Thread.create
          (fun () ->
            if K.Cancel.sleep watchdog_stop 5. then (
              K.Cancel.cancel cancel_a;
              K.Cancel.cancel cancel_b))
          ()
      in
      Thread.join thread_a;
      Thread.join thread_b;
      K.Cancel.cancel watchdog_stop;
      Thread.join watchdog;
      Atomic.set stop_server true;
      K.Client.close client_a;
      K.Client.close client_b;
      Thread.join server;
      Option.iter Alcotest.fail (Atomic.get server_error);
      let check_finished = function
        | Some (Ok (K.Leader_election.Finished identity)) -> identity
        | Some (Ok K.Leader_election.Cancelled_before_leadership) ->
            Alcotest.fail "candidate stopped before leadership"
        | Some (Error error) ->
            Alcotest.fail
              (Format.asprintf "%a" K.Leader_election.pp_error error)
        | None -> Alcotest.fail "candidate thread produced no result"
      in
      Alcotest.(check string)
        "candidate A completed" "candidate-a" (check_finished !result_a);
      Alcotest.(check string)
        "candidate B completed" "candidate-b" (check_finished !result_b);
      Mutex.lock order_lock;
      let leaders = List.rev !leader_order in
      Mutex.unlock order_lock;
      Alcotest.(check int) "two successive leaders" 2 (List.length leaders);
      Alcotest.(check bool)
        "both identities led" true
        (List.sort String.compare leaders = [ "candidate-a"; "candidate-b" ]);
      Alcotest.(check int)
        "protected work never overlapped" 1
        (Atomic.get maximum_active))

let test_http_body_limit () =
  let result, _requests =
    with_server
      [ response "12345" ]
      (fun _port config ->
        K.Http.request_once ~max_body_bytes:4 config `GET "/too-large")
  in
  match result with
  | Ok _ -> Alcotest.fail "oversized body was accepted"
  | Error message ->
      Alcotest.(check bool)
        "reports limit" true
        (String.starts_with ~prefix:"HTTP body exceeds 4 bytes" message)

let test_http_request_validation () =
  let config = K.Config.make (Uri.of_string "http://127.0.0.1:1") in
  Alcotest.(check bool)
    "reject unencoded whitespace" true
    (Result.is_error (K.Http.request_once config `GET "/bad path"));
  Alcotest.(check bool)
    "reject managed header" true
    (Result.is_error
       (K.Http.request_once
          ~headers:[ ("Content-Length", "99") ]
          config `GET "/"));
  Alcotest.(check bool)
    "reject invalid header name" true
    (Result.is_error
       (K.Http.request_once
          ~headers:[ ("Bad Header", "value") ]
          config `GET "/"));
  Alcotest.check_raises "reject invalid connection timeout"
    (Invalid_argument
       "Http.request_once: connect_timeout must be finite and positive")
    (fun () -> ignore (K.Http.request_once ~connect_timeout:0. config `GET "/"));
  Alcotest.check_raises "reject invalid write timeout"
    (Invalid_argument
       "Http.request_once: write_timeout must be finite and positive")
    (fun () -> ignore (K.Http.request_once ~write_timeout:0. config `GET "/"));
  Alcotest.check_raises "reject invalid response header timeout"
    (Invalid_argument
       "Http.request_once: response_header_timeout must be finite and positive")
    (fun () ->
      ignore (K.Http.request_once ~response_header_timeout:0. config `GET "/"))

let test_paginated_list () =
  let page_one =
    list_json ~continue:"next page" ~remaining:1 ~resource_version:"17"
      [ widget_json ~name:"first" ~resource_version:"11" () ]
    |> Yojson.Safe.to_string |> response
  in
  let page_two =
    list_json ~remaining:0 ~resource_version:"17"
      [ widget_json ~name:"second" ~resource_version:"12" () ]
    |> Yojson.Safe.to_string |> response
  in
  let result, requests =
    with_server [ page_one; page_two ] (fun _port config ->
        Widget_api.list_all ~page_size:1 (K.Client.create config))
  in
  (match result with
  | Error error -> Alcotest.fail (Format.asprintf "%a" K.Client.pp_error error)
  | Ok page ->
      Alcotest.(check int) "two aggregated items" 2 (List.length page.items);
      Alcotest.(check string)
        "snapshot version" "17"
        (K.Core.Resource_version.to_string page.resource_version));
  (match List.map request_target requests with
  | [ first; second ] ->
      let first = Uri.of_string first and second = Uri.of_string second in
      Alcotest.(check (option string))
        "first page limit" (Some "1")
        (Uri.get_query_param first "limit");
      Alcotest.(check (option string))
        "continuation token encoded and recovered" (Some "next page")
        (Uri.get_query_param second "continue")
  | _ -> Alcotest.fail "pagination did not make exactly two requests");
  let repeating_page =
    list_json ~continue:"same" ~resource_version:"17" []
    |> Yojson.Safe.to_string |> response
  in
  let result, _ =
    with_server [ repeating_page; repeating_page ] (fun _port config ->
        Widget_api.list_all ~page_size:1 (K.Client.create config))
  in
  Alcotest.(check bool)
    "repeated continuation token is rejected" true (Result.is_error result)

let test_dynamic_list_type_meta_defaulting () =
  let response =
    `Assoc
      [
        ("metadata", `Assoc [ ("resourceVersion", `String "21") ]);
        ( "items",
          `List
            [
              `Assoc
                [
                  ( "metadata",
                    `Assoc
                      [
                        ("name", `String "without-type-meta");
                        ("namespace", `String "default");
                        ("resourceVersion", `String "20");
                      ] );
                ];
            ] );
      ]
    |> Yojson.Safe.to_string
  in
  Fake_api_server.with_server
    [ Fake_api_server.fixed response ]
    (fun server ->
      let client = K.Client.create (Fake_api_server.config server) in
      Fun.protect
        ~finally:(fun () -> K.Client.close client)
        (fun () ->
          let page =
            match
              K.Dynamic.list_all ~namespace:"default" client ~api:widget_api
            with
            | Ok page -> page
            | Error error ->
                Alcotest.fail
                  ("dynamic list without TypeMeta: "
                  ^ Format.asprintf "%a" K.Client.pp_error error)
          in
          match page.items with
          | [ item ] ->
              Alcotest.(check string)
                "defaulted API version" "testing.ocaml-k8s.dev/v1"
                item.K.Dynamic.api_version;
              Alcotest.(check string)
                "defaulted kind" "Widget" item.K.Dynamic.kind
          | items ->
              Alcotest.fail
                (Printf.sprintf "expected one dynamic item, got %d"
                   (List.length items))))

let scale_json ?(status_replicas = 0l) ?selector replicas =
  `Assoc
    [
      ("apiVersion", `String "autoscaling/v1");
      ("kind", `String "Scale");
      ( "metadata",
        `Assoc
          [
            ("name", `String "one");
            ("namespace", `String "operators");
            ("resourceVersion", `String "51");
          ] );
      ("spec", `Assoc [ ("replicas", `Int (Int32.to_int replicas)) ]);
      ( "status",
        `Assoc
          ([ ("replicas", `Int (Int32.to_int status_replicas)) ]
          @
          match selector with
          | None -> []
          | Some value -> [ ("selector", `String value) ]) );
    ]

let require_client_ok context = function
  | Ok value -> value
  | Error error ->
      Alcotest.fail
        (context ^ ": " ^ Format.asprintf "%a" K.Client.pp_error error)

let test_collection_subresources_scale_and_logs () =
  let json value = Yojson.Safe.to_string value in
  Fake_api_server.with_server
    [
      Fake_api_server.fixed "{}";
      Fake_api_server.fixed "{}";
      Fake_api_server.fixed {|{"accepted":true}|};
      Fake_api_server.fixed {|{"updated":true}|};
      Fake_api_server.fixed "{}";
      Fake_api_server.fixed (json (scale_json ~status_replicas:2l 3l));
      Fake_api_server.fixed (json (scale_json ~status_replicas:5l 5l));
      Fake_api_server.fixed (json (scale_json ~status_replicas:6l 6l));
      Fake_api_server.fixed
        ~headers:[ ("Content-Type", "text/plain") ]
        "first\nsecond\n";
      Fake_api_server.chunked
        ~headers:[ ("Content-Type", "text/plain") ]
        [ "third\n"; "fourth\n" ];
    ]
    (fun server ->
      let client = K.Client.create (Fake_api_server.config server) in
      Fun.protect
        ~finally:(fun () -> K.Client.close client)
        (fun () ->
          let delete_options =
            {
              K.Client.default_delete_options with
              grace_period_seconds = Some 7;
              propagation_policy = Some `Foreground;
              dry_run = [ "All" ];
            }
          in
          Widget_api.delete_collection ~options:delete_options
            ~label_selector:"app=demo" ~field_selector:"metadata.name=one"
            ~resource_version:(K.Core.Resource_version.of_string "40")
            ~resource_version_match:`Exact ~limit:25 ~continue:"next token"
            ~timeout_seconds:9 client
          |> require_client_ok "default-namespace collection delete";
          Widget_api.delete_collection ~all_namespaces:true
            ~label_selector:"app=demo" client
          |> require_client_ok "all-namespaces collection delete";
          let write_options =
            {
              K.Client.default_write_options with
              field_manager = Some "operator/tests";
              field_validation = Some `Strict;
            }
          in
          Widget_api.create_subresource ~options:write_options client
            ~namespace:"operators" ~name:"one" ~subresource:"eviction"
            (`Assoc [ ("kind", `String "Eviction") ])
          |> require_client_ok "create subresource"
          |> ignore;
          Widget_api.replace_subresource client ~namespace:"operators"
            ~name:"one" ~subresource:"resize"
            (`Assoc [ ("kind", `String "PodResize") ])
          |> require_client_ok "replace subresource"
          |> ignore;
          Widget_api.delete_subresource client ~namespace:"operators" "one"
            "binding"
          |> require_client_ok "delete subresource";
          let scale =
            Widget_api.get_scale client ~namespace:"operators" "one"
            |> require_client_ok "get scale"
          in
          Alcotest.(check int32) "scale replicas" 3l scale.spec.replicas;
          Alcotest.(check (option string))
            "scale selector" None
            (Option.bind scale.status (fun status -> status.selector));
          let scale =
            { scale with K.Client.spec = { K.Client.replicas = 5l } }
          in
          let replaced =
            Widget_api.replace_scale client ~namespace:"operators" "one" scale
            |> require_client_ok "replace scale"
          in
          Alcotest.(check int32) "replaced scale" 5l replaced.spec.replicas;
          let patched =
            Widget_api.patch_scale client ~namespace:"operators" "one"
              (K.Client.Merge_patch
                 (`Assoc [ ("spec", `Assoc [ ("replicas", `Int 6) ]) ]))
            |> require_client_ok "patch scale"
          in
          Alcotest.(check int32) "patched scale" 6l patched.spec.replicas;
          let log_options =
            {
              K.Client.default_log_options with
              container = Some "main container";
              previous = true;
              since_seconds = Some 12;
              timestamps = true;
              tail_lines = Some 10L;
              limit_bytes = Some 4096L;
              insecure_skip_tls_verify_backend = true;
              stream = Some `Stdout;
            }
          in
          Alcotest.(check string)
            "buffered logs" "first\nsecond\n"
            (Widget_api.logs ~options:log_options client ~namespace:"operators"
               "one"
            |> require_client_ok "buffered logs");
          let streamed = Buffer.create 32 in
          let stream_options =
            {
              K.Client.default_log_options with
              container = Some "sidecar";
              follow = true;
              stream = Some `Stderr;
            }
          in
          Widget_api.stream_logs ~options:stream_options client
            ~namespace:"operators" "one"
            ~on_chunk:(Buffer.add_string streamed)
          |> require_client_ok "stream logs";
          Alcotest.(check string)
            "streamed log chunks" "third\nfourth\n" (Buffer.contents streamed));
      match Fake_api_server.requests server with
      | [
       delete_default;
       delete_all;
       create_subresource;
       replace_subresource;
       delete_subresource;
       get_scale;
       replace_scale;
       patch_scale;
       logs;
       stream_logs;
      ] ->
          let target request = Uri.of_string (request_target request) in
          Alcotest.(check string)
            "collection delete method" "DELETE"
            (request_method delete_default);
          Alcotest.(check string)
            "safe default collection scope"
            "/apis/testing.ocaml-k8s.dev/v1/namespaces/default/widgets"
            (Uri.path (target delete_default));
          Alcotest.(check (option string))
            "collection label selector" (Some "app=demo")
            (Uri.get_query_param (target delete_default) "labelSelector");
          Alcotest.(check (option string))
            "collection continuation" (Some "next token")
            (Uri.get_query_param (target delete_default) "continue");
          let delete_body =
            request_body delete_default |> Yojson.Safe.from_string
          in
          let delete_field name =
            match delete_body with
            | `Assoc fields -> List.assoc_opt name fields
            | _ -> None
          in
          Alcotest.(check bool)
            "delete propagation body" true
            (delete_field "propagationPolicy" = Some (`String "Foreground"));
          Alcotest.(check string)
            "explicit all-namespace collection scope"
            "/apis/testing.ocaml-k8s.dev/v1/widgets"
            (Uri.path (target delete_all));
          Alcotest.(check (list string))
            "subresource verbs"
            [ "POST"; "PUT"; "DELETE" ]
            (List.map request_method
               [ create_subresource; replace_subresource; delete_subresource ]);
          Alcotest.(check string)
            "create subresource path"
            "/apis/testing.ocaml-k8s.dev/v1/namespaces/operators/widgets/one/eviction"
            (Uri.path (target create_subresource));
          Alcotest.(check (option string))
            "subresource field manager" (Some "operator/tests")
            (Uri.get_query_param (target create_subresource) "fieldManager");
          Alcotest.(check (list string))
            "scale verbs" [ "GET"; "PUT"; "PATCH" ]
            (List.map request_method [ get_scale; replace_scale; patch_scale ]);
          Alcotest.(check string)
            "scale endpoint"
            "/apis/testing.ocaml-k8s.dev/v1/namespaces/operators/widgets/one/scale"
            (Uri.path (target get_scale));
          Alcotest.(check bool)
            "scale patch media type" true
            (contains_substring patch_scale
               "Content-Type: application/merge-patch+json");
          let log_target = target logs in
          Alcotest.(check string)
            "log endpoint"
            "/apis/testing.ocaml-k8s.dev/v1/namespaces/operators/widgets/one/log"
            (Uri.path log_target);
          Alcotest.(check (option string))
            "encoded container" (Some "main container")
            (Uri.get_query_param log_target "container");
          Alcotest.(check (option string))
            "tail lines" (Some "10")
            (Uri.get_query_param log_target "tailLines");
          Alcotest.(check (option string))
            "split stream" (Some "Stdout")
            (Uri.get_query_param log_target "stream");
          Alcotest.(check (option string))
            "follow stream" (Some "true")
            (Uri.get_query_param (target stream_logs) "follow")
      | requests ->
          Alcotest.fail
            (Printf.sprintf "expected ten client requests, got %d"
               (List.length requests)))

let test_log_validation_and_limits () =
  let zero_scale =
    scale_json 0l |> function
    | `Assoc fields ->
        `Assoc (("spec", `Assoc []) :: List.remove_assoc "spec" fields)
    | _ -> assert false
  in
  (match K.Client.scale_of_json zero_scale with
  | Ok scale ->
      Alcotest.(check int32) "omitted Scale replicas" 0l scale.spec.replicas
  | Error message -> Alcotest.fail ("zero Scale decode failed: " ^ message));
  Fake_api_server.with_server [] (fun server ->
      let client = K.Client.create (Fake_api_server.config server) in
      Fun.protect
        ~finally:(fun () -> K.Client.close client)
        (fun () ->
          let invalid =
            {
              K.Client.default_log_options with
              since_seconds = Some 1;
              since_time = Some "2026-08-30T00:00:00Z";
            }
          in
          (match Widget_api.logs ~options:invalid client "one" with
          | Error (K.Client.Invalid_request _) -> ()
          | Error error ->
              Alcotest.fail
                ("unexpected log validation error: "
                ^ Format.asprintf "%a" K.Client.pp_error error)
          | Ok _ -> Alcotest.fail "conflicting log times were accepted");
          Alcotest.(check bool)
            "empty subresource rejected" true
            (match Widget_api.get_subresource client "one" "" with
            | Error (K.Client.Invalid_request _) -> true
            | Ok _ | Error _ -> false));
      Alcotest.(check int)
        "invalid requests do not use HTTP" 0
        (List.length (Fake_api_server.requests server)));
  Fake_api_server.with_server
    [
      Fake_api_server.fixed ~headers:[ ("Content-Type", "text/plain") ] "12345";
    ]
    (fun server ->
      let client = K.Client.create (Fake_api_server.config server) in
      Fun.protect
        ~finally:(fun () -> K.Client.close client)
        (fun () ->
          match Widget_api.logs ~max_body_bytes:4 client "one" with
          | Error (K.Client.Transport message) ->
              Alcotest.(check bool)
                "bounded log error" true
                (String.starts_with ~prefix:"HTTP body exceeds 4 bytes" message)
          | Error error ->
              Alcotest.fail
                ("unexpected bounded log error: "
                ^ Format.asprintf "%a" K.Client.pp_error error)
          | Ok _ -> Alcotest.fail "oversized log response was accepted"));
  Fake_api_server.with_server
    [ Fake_api_server.fixed ~status:"500 Internal Server Error" "12345" ]
    (fun server ->
      let client = K.Client.create (Fake_api_server.config server) in
      Fun.protect
        ~finally:(fun () -> K.Client.close client)
        (fun () ->
          match
            Widget_api.stream_logs ~max_error_body_bytes:4 client "one"
              ~on_chunk:(fun _ -> ())
          with
          | Error (K.Client.Transport message) ->
              Alcotest.(check bool)
                "bounded streaming error" true
                (String.starts_with ~prefix:"HTTP body exceeds 4 bytes" message)
          | Error error ->
              Alcotest.fail
                ("unexpected streaming error limit result: "
                ^ Format.asprintf "%a" K.Client.pp_error error)
          | Ok () -> Alcotest.fail "oversized streaming error was accepted"));
  Fake_api_server.with_server
    [ Fake_api_server.delayed 0.2 (Fake_api_server.raw []) ]
    (fun server ->
      let client = K.Client.create (Fake_api_server.config server) in
      let cancel = K.Cancel.create () in
      let result = Atomic.make None in
      let started = Unix.gettimeofday () in
      let request =
        Thread.create
          (fun () ->
            Atomic.set result
              (Some
                 (Widget_api.stream_logs ~cancel client "one"
                    ~on_chunk:(fun _ -> ()))))
          ()
      in
      let deadline = Unix.gettimeofday () +. 1.0 in
      while
        Fake_api_server.requests server = [] && Unix.gettimeofday () < deadline
      do
        Unix.sleepf 0.001
      done;
      K.Cancel.cancel cancel;
      Thread.join request;
      K.Client.close client;
      (match Atomic.get result with
      | Some (Error (K.Client.Transport message)) ->
          Alcotest.(check bool)
            "stream cancellation classified" true
            (contains_substring message "cancelled")
      | Some (Error error) ->
          Alcotest.fail
            ("unexpected stream cancellation result: "
            ^ Format.asprintf "%a" K.Client.pp_error error)
      | Some (Ok ()) -> Alcotest.fail "cancelled stream completed normally"
      | None -> Alcotest.fail "cancelled stream produced no result");
      Alcotest.(check bool)
        "stream cancellation is prompt" true
        (Unix.gettimeofday () -. started < 0.5))

let test_api_error_contract () =
  let status ?reason ?retry_after code message =
    `Assoc
      ([
         ("apiVersion", `String "v1");
         ("kind", `String "Status");
         ("status", `String "Failure");
         ("message", `String message);
         ("code", `Int code);
       ]
      @ (match reason with
        | None -> []
        | Some reason -> [ ("reason", `String reason) ])
      @
      match retry_after with
      | None -> []
      | Some seconds ->
          [ ("details", `Assoc [ ("retryAfterSeconds", `Int seconds) ]) ])
    |> Yojson.Safe.to_string
  in
  Fake_api_server.with_server
    [
      Fake_api_server.fixed ~status:"404 Not Found"
        (status ~reason:"NotFound" 404 "missing");
      Fake_api_server.fixed ~status:"409 Conflict"
        (status ~reason:"AlreadyExists" 409 "exists");
      Fake_api_server.fixed ~status:"429 Too Many Requests"
        ~headers:[ ("Retry-After", "7") ]
        (status ~reason:"TooManyRequests" ~retry_after:3 429 "slow down");
      Fake_api_server.fixed ~status:"500 Internal Server Error"
        (status ~reason:"ServerTimeout" ~retry_after:11 500 "try later");
      Fake_api_server.fixed ~status:"429 Too Many Requests"
        (status ~reason:"FutureThrottleReason" 429 "future reason");
    ]
    (fun server ->
      let client = K.Client.create (Fake_api_server.config server) in
      Fun.protect
        ~finally:(fun () -> K.Client.close client)
        (fun () ->
          let get name =
            match Widget_api.get client name with
            | Error (K.Client.Api _ as error) -> error
            | Error error ->
                Alcotest.fail
                  ("unexpected client error: "
                  ^ Format.asprintf "%a" K.Client.pp_error error)
            | Ok _ -> Alcotest.fail "scripted Status response was accepted"
          in
          let missing = get "missing" in
          Alcotest.(check bool)
            "NotFound reason" true
            (K.Client.Error.is_not_found missing);
          Alcotest.(check (option int))
            "status code" (Some 404)
            (K.Client.Error.status_code missing);
          Alcotest.(check (option string))
            "status reason" (Some "NotFound")
            (K.Client.Error.reason missing);
          let exists = get "exists" in
          Alcotest.(check bool)
            "AlreadyExists reason" true
            (K.Client.Error.is_already_exists exists);
          Alcotest.(check bool)
            "known 409 reason is not Conflict" false
            (K.Client.Error.is_conflict exists);
          let throttled = get "throttled" in
          Alcotest.(check bool)
            "TooManyRequests reason" true
            (K.Client.Error.is_too_many_requests throttled);
          Alcotest.(check bool)
            "throttling is transient" true
            (K.Client.Error.is_transient throttled);
          Alcotest.(check (option (float 0.)))
            "Retry-After header takes precedence" (Some 7.)
            (K.Client.Error.suggested_delay throttled);
          let timed_out = get "timed-out" in
          Alcotest.(check bool)
            "ServerTimeout reason" true
            (K.Client.Error.is_server_timeout timed_out);
          Alcotest.(check (option (float 0.)))
            "Status details delay" (Some 11.)
            (K.Client.Error.suggested_delay timed_out);
          let future = get "future" in
          Alcotest.(check bool)
            "unknown reason falls back to code" true
            (K.Client.Error.is_too_many_requests future)))

let test_event_recorder () =
  let fixed_now =
    match Ptime.of_rfc3339 "2026-08-30T12:34:56Z" with
    | Ok (time, _, _) -> time
    | Error _ -> Alcotest.fail "invalid fixed event timestamp"
  in
  let response =
    `Assoc
      [
        ("apiVersion", `String "events.k8s.io/v1");
        ("kind", `String "Event");
        ( "metadata",
          `Assoc
            [
              ("name", `String "stored-event");
              ("namespace", `String "operators");
              ("resourceVersion", `String "9");
            ] );
        ("eventTime", `String "2026-08-30T12:34:56.000000Z");
      ]
    |> Yojson.Safe.to_string
  in
  Fake_api_server.with_server
    [ Fake_api_server.fixed response ]
    (fun server ->
      let client = K.Client.create (Fake_api_server.config server) in
      Fun.protect
        ~finally:(fun () -> K.Client.close client)
        (fun () ->
          let recorder =
            match
              K.Events.create ~client ~namespace:"operator-system"
                ~reporting_controller:"demo.ocaml-k8s.dev/greeting-controller"
                ~reporting_instance:"greeting-operator-pod-7"
                ~now:(fun () -> fixed_now)
                ()
            with
            | Ok recorder -> recorder
            | Error message -> Alcotest.fail message
          in
          let metadata =
            {
              K.Core.name = "widget";
              namespace = Some "operators";
              uid = Some "widget-uid";
              resource_version = Some (K.Core.Resource_version.of_string "17");
              generation = Some 2;
              deletion_timestamp = None;
              finalizers = [];
              owner_references = [];
              labels = [];
              annotations = [];
            }
          in
          let regarding = K.Core.object_reference widget_api metadata in
          (match
             K.Events.record recorder ~regarding ~type_:K.Events.Warning
               ~reason:"ReconcileFailed" ~action:"Reconcile"
               ~note:"dependent Deployment was unavailable"
           with
          | Ok () -> ()
          | Error error ->
              Alcotest.fail (Format.asprintf "%a" K.Client.pp_error error));
          Alcotest.(check bool)
            "oversized note rejected" true
            (match
               K.Events.record recorder ~regarding ~type_:K.Events.Normal
                 ~reason:"Ready" ~action:"Reconcile"
                 ~note:(String.make 1025 'x')
             with
            | Error (K.Client.Invalid_request _) -> true
            | Ok () | Error _ -> false));
      match Fake_api_server.requests server with
      | [ request ] ->
          Alcotest.(check string) "event method" "POST" (request_method request);
          Alcotest.(check string)
            "event namespace follows regarding object"
            "/apis/events.k8s.io/v1/namespaces/operators/events"
            (request_target request);
          let body = request_body request |> Yojson.Safe.from_string in
          let member name = function
            | `Assoc fields -> List.assoc_opt name fields
            | _ -> None
          in
          let string_member name json =
            Option.bind (member name json) (function
              | `String value -> Some value
              | _ -> None)
          in
          Alcotest.(check (option string))
            "event type" (Some "Warning")
            (string_member "type" body);
          Alcotest.(check (option string))
            "event timestamp" (Some "2026-08-30T12:34:56.000000Z")
            (string_member "eventTime" body);
          let metadata =
            match member "metadata" body with
            | Some value -> value
            | None -> Alcotest.fail "Event request has no metadata"
          in
          let event_name =
            match string_member "name" metadata with
            | Some value -> value
            | None -> Alcotest.fail "Event request has no name"
          in
          Alcotest.(check bool)
            "event name is DNS-sized" true
            (String.starts_with ~prefix:"widget." event_name
            && String.length event_name <= 253);
          let regarding =
            match member "regarding" body with
            | Some value -> value
            | None -> Alcotest.fail "Event request has no regarding reference"
          in
          Alcotest.(check (option string))
            "regarding UID" (Some "widget-uid")
            (string_member "uid" regarding)
      | requests ->
          Alcotest.fail
            (Printf.sprintf "expected one Event request, got %d"
               (List.length requests)))

let test_watch_bookmark_and_expiry () =
  let event event_type object_json =
    `Assoc [ ("type", `String event_type); ("object", object_json) ]
    |> Yojson.Safe.to_string
  in
  let stream =
    String.concat "\n"
      [
        event "ADDED" (widget_json ~name:"watched" ~resource_version:"18" ());
        event "BOOKMARK"
          (`Assoc [ ("metadata", `Assoc [ ("resourceVersion", `String "19") ]) ]);
        "";
      ]
  in
  let seen = ref [] in
  let result, _ =
    with_server
      [ response stream ]
      (fun _port config ->
        Widget_api.watch (K.Client.create config)
          ~resource_version:(K.Core.Resource_version.of_string "17")
          ~on_event:(fun event -> seen := event :: !seen))
  in
  (match result with
  | Ok (K.Client.Watch_ended version) ->
      Alcotest.(check string)
        "bookmark advances watch" "19"
        (K.Core.Resource_version.to_string version)
  | Ok K.Client.Resource_version_expired ->
      Alcotest.fail "ordinary watch was treated as expired"
  | Error error -> Alcotest.fail (Format.asprintf "%a" K.Client.pp_error error));
  Alcotest.(check int) "added and bookmark delivered" 2 (List.length !seen);
  let gone =
    event "ERROR"
      (`Assoc
         [
           ("apiVersion", `String "v1");
           ("kind", `String "Status");
           ("reason", `String "Expired");
           ("message", `String "too old resource version");
           ("code", `Int 410);
         ])
    ^ "\n"
  in
  let result, _ =
    with_server
      [ response gone ]
      (fun _port config ->
        Widget_api.watch (K.Client.create config)
          ~resource_version:(K.Core.Resource_version.of_string "17")
          ~on_event:(fun _ -> ()))
  in
  Alcotest.(check bool)
    "in-band 410 requests relist" true
    (result = Ok K.Client.Resource_version_expired);
  let throttled =
    event "ERROR"
      (`Assoc
         [
           ("apiVersion", `String "v1");
           ("kind", `String "Status");
           ("reason", `String "TooManyRequests");
           ("message", `String "slow down");
           ("details", `Assoc [ ("retryAfterSeconds", `Int 4) ]);
           ("code", `Int 429);
         ])
    ^ "\n"
  in
  let observed_status = Atomic.make false in
  let observed_delay = Atomic.make None in
  let result, _ =
    with_server
      [ response throttled ]
      (fun _port config ->
        Widget_api.watch (K.Client.create config)
          ~resource_version:(K.Core.Resource_version.of_string "19")
          ~on_event:(function
          | K.Client.Watch_error error when error.code = 429 ->
              Atomic.set observed_status true;
              Atomic.set observed_delay error.retry_after_seconds
          | _ -> ()))
  in
  (match result with
  | Error (K.Client.Api error) ->
      Alcotest.(check int) "in-band Status code" 429 error.code
  | Error error ->
      Alcotest.fail
        ("unexpected watch error: "
        ^ Format.asprintf "%a" K.Client.pp_error error)
  | Ok _ -> Alcotest.fail "non-410 watch Status was treated as a clean EOF");
  Alcotest.(check bool)
    "Status event delivered before reconnect error" true
    (Atomic.get observed_status);
  Alcotest.(check (option int))
    "in-band Status retry delay" (Some 4)
    (Atomic.get observed_delay)

let watch_event event_type object_json =
  `Assoc [ ("type", `String event_type); ("object", object_json) ]
  |> Yojson.Safe.to_string

let bytes value =
  List.init (String.length value) (fun index -> String.make 1 value.[index])

let test_watch_framing_faults () =
  let added =
    watch_event "ADDED" (widget_json ~name:"framed" ~resource_version:"61" ())
  in
  let modified =
    watch_event "MODIFIED"
      (widget_json ~name:"framed" ~resource_version:"62" ())
  in
  let bookmark =
    watch_event "BOOKMARK"
      (`Assoc [ ("metadata", `Assoc [ ("resourceVersion", `String "63") ]) ])
  in
  let coalesced = added ^ "\n" ^ modified ^ "\n" in
  let fragmented = bookmark ^ "\n" in
  let max_event_bytes = max (String.length added) (String.length modified) in
  let gone =
    `Assoc
      [
        ("apiVersion", `String "v1");
        ("kind", `String "Status");
        ("reason", `String "Expired");
        ("message", `String "resource version is too old");
        ("code", `Int 410);
      ]
    |> Yojson.Safe.to_string
  in
  Fake_api_server.with_server
    [
      Fake_api_server.chunked [ coalesced ];
      Fake_api_server.chunked (bytes fragmented);
      Fake_api_server.chunked ~terminate:false [ added ^ "\n" ];
      Fake_api_server.chunked ~terminate:false [ added ^ "\n" ];
      Fake_api_server.fixed ~status:"410 Gone" gone;
    ]
    (fun server ->
      let client = K.Client.create (Fake_api_server.config server) in
      Fun.protect
        ~finally:(fun () -> K.Client.close client)
        (fun () ->
          let first_events = ref [] in
          let first =
            Widget_api.watch ~max_event_bytes client
              ~resource_version:(K.Core.Resource_version.of_string "60")
              ~on_event:(fun event -> first_events := event :: !first_events)
          in
          (match first with
          | Ok (K.Client.Watch_ended version) ->
              Alcotest.(check string)
                "coalesced stream version" "62"
                (K.Core.Resource_version.to_string version)
          | Ok K.Client.Resource_version_expired ->
              Alcotest.fail "coalesced stream expired"
          | Error error ->
              Alcotest.fail (Format.asprintf "%a" K.Client.pp_error error));
          Alcotest.(check int)
            "coalesced events are bounded individually" 2
            (List.length !first_events);
          let second =
            Widget_api.watch ~max_event_bytes client
              ~resource_version:(K.Core.Resource_version.of_string "62")
              ~on_event:(fun _ -> ())
          in
          (match second with
          | Ok (K.Client.Watch_ended version) ->
              Alcotest.(check string)
                "byte-fragmented stream version" "63"
                (K.Core.Resource_version.to_string version)
          | Ok K.Client.Resource_version_expired ->
              Alcotest.fail "fragmented stream expired"
          | Error error ->
              Alcotest.fail (Format.asprintf "%a" K.Client.pp_error error));
          let oversized =
            Widget_api.watch
              ~max_event_bytes:(String.length added - 1)
              client
              ~resource_version:(K.Core.Resource_version.of_string "60")
              ~on_event:(fun _ -> ())
          in
          (match oversized with
          | Error (K.Client.Decode message) ->
              Alcotest.(check bool)
                "individual event limit is reported" true
                (String.starts_with ~prefix:"watch event exceeds" message)
          | Error error ->
              Alcotest.fail
                ("unexpected oversized-event error: "
                ^ Format.asprintf "%a" K.Client.pp_error error)
          | Ok _ -> Alcotest.fail "oversized watch event was accepted");
          let delivered_before_truncation = Atomic.make 0 in
          let truncated =
            Widget_api.watch client
              ~resource_version:(K.Core.Resource_version.of_string "60")
              ~on_event:(fun _ -> Atomic.incr delivered_before_truncation)
          in
          (match truncated with
          | Error (K.Client.Transport _) -> ()
          | Error error ->
              Alcotest.fail
                ("unexpected truncated-watch error: "
                ^ Format.asprintf "%a" K.Client.pp_error error)
          | Ok _ -> Alcotest.fail "unterminated chunked watch looked complete");
          Alcotest.(check int)
            "complete event delivered before framing failure" 1
            (Atomic.get delivered_before_truncation);
          let expired =
            Widget_api.watch client
              ~resource_version:(K.Core.Resource_version.of_string "1")
              ~on_event:(fun _ -> ())
          in
          Alcotest.(check bool)
            "HTTP 410 requests a relist" true
            (expired = Ok K.Client.Resource_version_expired)))

let test_reflector_resumes_after_truncated_watch () =
  let snapshot =
    list_json ~resource_version:"70"
      [ widget_json ~name:"resumed" ~resource_version:"70" () ]
    |> Yojson.Safe.to_string
  in
  let modified version =
    watch_event "MODIFIED"
      (widget_json ~name:"resumed" ~resource_version:version ())
    ^ "\n"
  in
  Fake_api_server.with_server
    [
      Fake_api_server.fixed snapshot;
      Fake_api_server.chunked ~terminate:false [ modified "71" ];
      Fake_api_server.chunked ~terminate:false [ modified "72" ];
    ]
    (fun server ->
      let client = K.Client.create (Fake_api_server.config server) in
      let cancel = K.Cancel.create () in
      let reflector = Widget_controller.Cache.create () in
      let latest = Atomic.make "" in
      let unsubscribe =
        Widget_controller.Cache.subscribe reflector (function
          | Widget_controller.Cache.Added value ->
              Option.iter
                (fun version ->
                  Atomic.set latest (K.Core.Resource_version.to_string version))
                value.K.Dynamic.metadata.resource_version
          | Widget_controller.Cache.Modified { current; _ } ->
              Option.iter
                (fun version ->
                  Atomic.set latest (K.Core.Resource_version.to_string version))
                current.K.Dynamic.metadata.resource_version
          | Widget_controller.Cache.Deleted _ -> ())
      in
      let result = Atomic.make None in
      let runner =
        Thread.create
          (fun () ->
            let manager = K.Manager.create ~cancel client in
            K.Manager.add manager (Widget_controller.Cache.component reflector);
            Atomic.set result (Some (K.Manager.run manager)))
          ()
      in
      let deadline = Unix.gettimeofday () +. 5.0 in
      while Atomic.get latest <> "72" && Unix.gettimeofday () < deadline do
        Unix.sleepf 0.001
      done;
      if Atomic.get latest <> "72" then
        Alcotest.fail "reflector did not resume after truncated watch";
      K.Cancel.cancel cancel;
      Thread.join runner;
      unsubscribe ();
      K.Client.close client;
      (match Atomic.get result with
      | Some (Ok ()) -> ()
      | Some (Error error) ->
          Alcotest.fail (Format.asprintf "%a" K.Manager.pp_error error)
      | None -> Alcotest.fail "reflector produced no result");
      let key = K.Core.Object_key.make ~namespace:"default" "resumed" in
      let cached =
        match Widget_controller.Cache.get reflector key with
        | None -> Alcotest.fail "resumed resource is absent from cache"
        | Some value -> value
      in
      Alcotest.(check (option string))
        "latest object committed to cache" (Some "72")
        (Option.map K.Core.Resource_version.to_string
           cached.K.Dynamic.metadata.resource_version);
      match List.map request_target (Fake_api_server.requests server) with
      | [ listed; first_watch; second_watch ] ->
          Alcotest.(check (option string))
            "first watch starts at snapshot" (Some "70")
            (Uri.get_query_param (Uri.of_string first_watch) "resourceVersion");
          Alcotest.(check (option string))
            "reconnect resumes after delivered event" (Some "71")
            (Uri.get_query_param (Uri.of_string second_watch) "resourceVersion");
          Alcotest.(check (option string))
            "initial request is a list" None
            (Uri.get_query_param (Uri.of_string listed) "watch")
      | requests ->
          Alcotest.fail
            (Printf.sprintf "expected LIST and two WATCHes, got %d requests"
               (List.length requests)))

let test_reflector_compaction_relist () =
  let first_snapshot =
    list_json ~resource_version:"80"
      [
        widget_json ~name:"stable" ~resource_version:"80" ();
        widget_json ~name:"obsolete" ~resource_version:"80" ();
      ]
    |> Yojson.Safe.to_string
  in
  let expired =
    watch_event "ERROR"
      (`Assoc
         [
           ("apiVersion", `String "v1");
           ("kind", `String "Status");
           ("reason", `String "Expired");
           ("message", `String "compacted");
           ("code", `Int 410);
         ])
    ^ "\n"
  in
  let second_snapshot =
    list_json ~resource_version:"90"
      [
        widget_json ~name:"stable" ~resource_version:"90" ();
        widget_json ~name:"current" ~resource_version:"90" ();
      ]
    |> Yojson.Safe.to_string
  in
  Fake_api_server.with_server
    [
      Fake_api_server.fixed first_snapshot;
      Fake_api_server.chunked [ expired ];
      Fake_api_server.fixed second_snapshot;
    ]
    (fun server ->
      let client = K.Client.create (Fake_api_server.config server) in
      let cancel = K.Cancel.create () in
      let reflector = Widget_controller.Cache.create () in
      let relisted_transition = Atomic.make None in
      let unsubscribe =
        Widget_controller.Cache.subscribe reflector (function
          | Widget_controller.Cache.Modified
              { previous = Some previous; current }
            when current.K.Dynamic.metadata.name = "stable" ->
              let version value =
                Option.map K.Core.Resource_version.to_string
                  value.K.Dynamic.metadata.resource_version
              in
              Atomic.set relisted_transition
                (Some (version previous, version current))
          | Widget_controller.Cache.Added value
            when value.K.Dynamic.metadata.name = "current" ->
              K.Cancel.cancel cancel
          | _ -> ())
      in
      let manager = K.Manager.create ~cancel client in
      K.Manager.add manager (Widget_controller.Cache.component reflector);
      let result = K.Manager.run manager in
      unsubscribe ();
      K.Client.close client;
      (match result with
      | Ok () -> ()
      | Error error ->
          Alcotest.fail (Format.asprintf "%a" K.Manager.pp_error error));
      let obsolete = K.Core.Object_key.make ~namespace:"default" "obsolete" in
      let current = K.Core.Object_key.make ~namespace:"default" "current" in
      let stable = K.Core.Object_key.make ~namespace:"default" "stable" in
      Alcotest.(check bool)
        "compacted snapshot removed" true
        (Widget_controller.Cache.get reflector obsolete = None);
      Alcotest.(check bool)
        "fresh snapshot installed" true
        (Widget_controller.Cache.get reflector current <> None);
      Alcotest.(check bool)
        "overlapping resource retained" true
        (Widget_controller.Cache.get reflector stable <> None);
      Alcotest.(check (option (pair (option string) (option string))))
        "relist preserves old and new relationship inputs"
        (Some (Some "80", Some "90"))
        (Atomic.get relisted_transition);
      match List.map request_target (Fake_api_server.requests server) with
      | [ first_list; watched; second_list ] ->
          Alcotest.(check (option string))
            "watch starts at first snapshot" (Some "80")
            (Uri.get_query_param (Uri.of_string watched) "resourceVersion");
          List.iter
            (fun listed ->
              Alcotest.(check (option string))
                "compaction performs unconditional relist" None
                (Uri.get_query_param (Uri.of_string listed) "resourceVersion"))
            [ first_list; second_list ]
      | requests ->
          Alcotest.fail
            (Printf.sprintf "expected LIST/WATCH/LIST, got %d requests"
               (List.length requests)))

let test_patch_and_finalizer_requests () =
  let patched =
    widget_json ~name:"patched" ~resource_version:"21" ()
    |> Yojson.Safe.to_string |> response
  in
  let result, requests =
    with_server [ patched ] (fun _port config ->
        Widget_api.patch
          ~options:
            {
              K.Client.default_write_options with
              dry_run = [ "All" ];
              field_validation = Some `Strict;
            }
          (K.Client.create config) "patched"
          (K.Client.Apply
             {
               value = widget_json ~name:"patched" ~resource_version:"20" ();
               field_manager = "ocaml-k8s-test";
               force = true;
             }))
  in
  (match result with
  | Ok _ -> ()
  | Error error -> Alcotest.fail (Format.asprintf "%a" K.Client.pp_error error));
  let request = List.hd requests in
  let target = request_target request |> Uri.of_string in
  Alcotest.(check (option string))
    "default object namespace"
    (Some "/apis/testing.ocaml-k8s.dev/v1/namespaces/default/widgets/patched")
    (Some (Uri.path target));
  Alcotest.(check (option string))
    "apply manager" (Some "ocaml-k8s-test")
    (Uri.get_query_param target "fieldManager");
  Alcotest.(check (option string))
    "apply force" (Some "true")
    (Uri.get_query_param target "force");
  Alcotest.(check bool)
    "apply content type" true
    (String.split_on_char '\n' request
    |> List.exists (fun line ->
        String.trim line = "Content-Type: application/apply-patch+yaml"));
  let without_version =
    match
      K.Dynamic.of_json (widget_json ~name:"patched" ~resource_version:"" ())
    with
    | Error message -> Alcotest.fail message
    | Ok value ->
        {
          value with
          metadata = { value.metadata with resource_version = None };
        }
  in
  let finalizer_response =
    widget_json ~name:"patched" ~resource_version:"22" ()
    |> Yojson.Safe.to_string |> response
  in
  let finalizer = "testing.ocaml-k8s.dev/finalizer" in
  let callback_called = Atomic.make false in
  let result, requests =
    with_server [ finalizer_response ] (fun _port config ->
        Widget_finalizer.run (K.Client.create config) without_version finalizer
          (fun _ ->
            Atomic.set callback_called true;
            Ok K.Controller.Done))
  in
  (match result with
  | Ok K.Controller.Requeue -> ()
  | Ok _ -> Alcotest.fail "new finalizer did not request a fresh reconcile"
  | Error error -> Alcotest.fail error);
  Alcotest.(check bool)
    "callback waits for persisted finalizer" false
    (Atomic.get callback_called);
  let body = List.hd requests |> request_body |> Yojson.Safe.from_string in
  let metadata =
    match body with
    | `Assoc fields -> List.assoc "metadata" fields
    | _ -> Alcotest.fail "finalizer patch is not a JSON object"
  in
  Alcotest.(check bool)
    "missing resource version is omitted, not null" false
    (match metadata with
    | `Assoc fields -> List.mem_assoc "resourceVersion" fields
    | _ -> Alcotest.fail "finalizer patch metadata is not an object");
  let deleting =
    {
      without_version with
      metadata =
        {
          without_version.metadata with
          resource_version = Some (K.Core.Resource_version.of_string "22");
          deletion_timestamp = Some "2026-08-30T00:00:00Z";
          finalizers = [ finalizer ];
        };
    }
  in
  let cleanup_called = Atomic.make false in
  let removed =
    widget_json ~name:"patched" ~resource_version:"23" ()
    |> Yojson.Safe.to_string |> response
  in
  let result, requests =
    with_server [ removed ] (fun _port config ->
        Widget_finalizer.run (K.Client.create config) deleting finalizer
          (function
          | Widget_finalizer.Cleanup _ ->
              Atomic.set cleanup_called true;
              Ok K.Controller.Done
          | Widget_finalizer.Apply _ ->
              Alcotest.fail "deleting object received Apply"))
  in
  (match result with
  | Ok K.Controller.Done -> ()
  | Ok _ -> Alcotest.fail "successful cleanup did not finish"
  | Error error -> Alcotest.fail error);
  Alcotest.(check bool)
    "cleanup callback called" true
    (Atomic.get cleanup_called);
  let body = List.hd requests |> request_body |> Yojson.Safe.from_string in
  let metadata =
    match body with
    | `Assoc fields -> List.assoc "metadata" fields
    | _ -> Alcotest.fail "finalizer removal is not a JSON object"
  in
  match metadata with
  | `Assoc fields ->
      Alcotest.(check (option string))
        "cleanup guards resource version" (Some "22")
        (match List.assoc_opt "resourceVersion" fields with
        | Some (`String value) -> Some value
        | _ -> None);
      Alcotest.(check int)
        "cleanup removes finalizer" 0
        (match List.assoc_opt "finalizers" fields with
        | Some (`List values) -> List.length values
        | _ -> Alcotest.fail "finalizers are not an array")
  | _ -> Alcotest.fail "finalizer removal metadata is not an object"

let exec_kubeconfig ?(api_version = "client.authentication.k8s.io/v1")
    ?(command = "/usr/bin/printf") ?args ?(interactive_mode = Some "Never") port
    output =
  let args = Option.value ~default:[ "%s"; output ] args in
  let interactive_mode =
    match interactive_mode with
    | None -> []
    | Some value -> [ ("interactiveMode", `String value) ]
  in
  `Assoc
    [
      ("apiVersion", `String "v1");
      ("kind", `String "Config");
      ( "clusters",
        `List
          [
            `Assoc
              [
                ("name", `String "local");
                ( "cluster",
                  `Assoc
                    [
                      ( "server",
                        `String (Printf.sprintf "http://127.0.0.1:%d" port) );
                    ] );
              ];
          ] );
      ( "contexts",
        `List
          [
            `Assoc
              [
                ("name", `String "local");
                ( "context",
                  `Assoc
                    [ ("cluster", `String "local"); ("user", `String "exec") ]
                );
              ];
          ] );
      ("current-context", `String "local");
      ( "users",
        `List
          [
            `Assoc
              [
                ("name", `String "exec");
                ( "user",
                  `Assoc
                    [
                      ( "exec",
                        `Assoc
                          ([
                             ("apiVersion", `String api_version);
                             ("command", `String command);
                             ( "args",
                               `List
                                 (List.map (fun value -> `String value) args) );
                           ]
                          @ interactive_mode) );
                    ] );
              ];
          ] );
    ]

let test_exec_credential_refresh () =
  let unauthorized =
    response ~status:"401 Unauthorized"
      {|{"kind":"Status","apiVersion":"v1","status":"Failure","message":"expired","reason":"Unauthorized","code":401}|}
  in
  let result, requests =
    with_server
      [ unauthorized; response "{}" ]
      (fun port _config ->
        let credential =
          {|{"apiVersion":"client.authentication.k8s.io/v1","kind":"ExecCredential","status":{"expirationTimestamp":"2099-01-01T00:00:00Z","token":"exec-token"}}|}
        in
        with_temp_file
          (Yojson.Safe.to_string (exec_kubeconfig port credential))
          (fun path ->
            match K.Config.load_kubeconfig path with
            | Error message -> Error (K.Client.Invalid_request message)
            | Ok config -> K.Client.raw (K.Client.create config) `GET "/version"))
  in
  (match result with
  | Error error -> Alcotest.fail (Format.asprintf "%a" K.Client.pp_error error)
  | Ok response -> Alcotest.(check int) "retried response" 200 response.status);
  Alcotest.(check int) "one retry after 401" 2 (List.length requests);
  List.iter
    (fun request ->
      Alcotest.(check bool)
        "exec bearer header" true
        (String.split_on_char '\n' request
        |> List.exists (fun line ->
            String.trim line = "Authorization: Bearer exec-token")))
    requests

let test_exec_credential_contract () =
  let load ?api_version ?interactive_mode () =
    let document =
      exec_kubeconfig ?api_version ?interactive_mode 1 "unused"
      |> Yojson.Safe.to_string
    in
    with_temp_file document K.Config.load_kubeconfig
  in
  Alcotest.(check bool)
    "v1 requires interactiveMode" true
    (Result.is_error (load ~interactive_mode:None ()));
  Alcotest.(check bool)
    "v1beta1 defaults interactiveMode" true
    (Result.is_ok
       (load ~api_version:"client.authentication.k8s.io/v1beta1"
          ~interactive_mode:None ()));
  Alcotest.(check bool)
    "reject unsupported exec version" true
    (Result.is_error
       (load ~api_version:"client.authentication.k8s.io/v1alpha1" ()))

let test_relative_exec_credential () =
  with_temp_directory (fun create ->
      let credential =
        {|{"apiVersion":"client.authentication.k8s.io/v1","kind":"ExecCredential","status":{"token":"relative-token"}}|}
      in
      let helper =
        create "credential-helper"
          (Printf.sprintf "#!/bin/sh\nprintf '%%s' '%s'\n" credential)
      in
      Unix.chmod helper 0o700;
      let kubeconfig =
        exec_kubeconfig ~command:"./credential-helper" ~args:[] 1 credential
        |> Yojson.Safe.to_string |> create "config.json"
      in
      match K.Config.load_kubeconfig kubeconfig with
      | Error message -> Alcotest.fail message
      | Ok config -> (
          match K.Config.authorization_header config with
          | Error message -> Alcotest.fail message
          | Ok value ->
              Alcotest.(check (option string))
                "exec path is relative to kubeconfig"
                (Some "Bearer relative-token") value))

let test_cancel () =
  let cancel = K.Cancel.create () in
  let called = Atomic.make 0 in
  let _unregister = K.Cancel.on_cancel cancel (fun () -> Atomic.incr called) in
  K.Cancel.cancel cancel;
  K.Cancel.cancel cancel;
  Alcotest.(check int) "callback once" 1 (Atomic.get called);
  Alcotest.(check bool) "cancelled sleep" false (K.Cancel.sleep cancel 0.01);
  let parent = K.Cancel.create () in
  let started = Unix.gettimeofday () in
  let slept, timed_out =
    K.Cancel.with_timeout ~parent 0.05 (fun child -> K.Cancel.sleep child 5.0)
  in
  Alcotest.(check bool) "deadline interrupts sleep" false slept;
  Alcotest.(check bool) "deadline is reported" true timed_out;
  Alcotest.(check bool)
    "deadline returns promptly" true
    (Unix.gettimeofday () -. started < 0.5)

let () =
  Alcotest.run "kube"
    [
      ( "core",
        [
          Alcotest.test_case "resource paths" `Quick test_paths;
          Alcotest.test_case "object references" `Quick test_object_references;
        ] );
      ( "config",
        [
          Alcotest.test_case "kind-style kubeconfig" `Quick test_kubeconfig;
          Alcotest.test_case "merged kubeconfigs" `Quick test_merged_kubeconfigs;
          Alcotest.test_case "proxy and impersonation kubeconfig" `Quick
            test_kubeconfig_proxy_and_impersonation;
        ] );
      ( "runtime",
        [
          Alcotest.test_case "work queue deduplication" `Quick
            test_queue_deduplication;
          Alcotest.test_case "scheduler shutdown" `Quick test_scheduler_shutdown;
          Alcotest.test_case "secondary store indexes" `Quick test_store_indexes;
          Alcotest.test_case "controller error policy" `Quick
            test_controller_error_policy;
          Alcotest.test_case "client rate limiter" `Quick test_rate_limiter;
          Alcotest.test_case "structured logging" `Quick test_structured_logging;
          Alcotest.test_case "logging integration" `Quick
            test_logging_integration;
          Alcotest.test_case "cache-backed client readiness" `Quick
            test_cached_client_readiness;
          Alcotest.test_case "manager dependency deduplication" `Quick
            test_manager_dependency_deduplication;
          Alcotest.test_case "manager failure supervision" `Quick
            test_manager_failure_cancels_siblings;
          Alcotest.test_case "Prometheus metrics registry" `Quick
            test_metrics_registry;
          Alcotest.test_case "Lease leader-election lifecycle" `Quick
            test_leader_election_lifecycle;
          Alcotest.test_case "Lease leader-election contention" `Quick
            test_leader_election_contention;
          Alcotest.test_case "cancellation" `Quick test_cancel;
        ] );
      ( "http",
        [
          Alcotest.test_case "chunked streaming" `Quick test_streaming_http;
          Alcotest.test_case "connection establishment timeout" `Quick
            test_connection_establishment_timeout;
          Alcotest.test_case "connection establishment cancellation" `Quick
            test_connection_establishment_cancellation;
          Alcotest.test_case "request write timeout" `Quick
            test_request_write_timeout;
          Alcotest.test_case "response header timeout" `Quick
            test_response_header_timeout;
          Alcotest.test_case "HTTP forward proxy" `Quick test_http_forward_proxy;
          Alcotest.test_case "HTTP CONNECT proxy rejection" `Quick
            test_http_connect_proxy_failure;
          Alcotest.test_case "SOCKS5 proxy" `Quick test_socks5_proxy;
          Alcotest.test_case "client impersonation" `Quick
            test_client_impersonation;
          Alcotest.test_case "persistent connection reuse" `Quick
            test_http_connection_reuse;
          Alcotest.test_case "shared reflector cache" `Quick
            test_shared_reflector_cache;
          Alcotest.test_case "controller cache-sync timeout" `Quick
            test_controller_cache_sync_timeout;
          Alcotest.test_case "controller reconcile timeout" `Quick
            test_controller_reconcile_timeout;
          Alcotest.test_case "cache-backed live delegation" `Quick
            test_cached_client_live_delegation;
          Alcotest.test_case "owned resource watch" `Quick
            test_owned_resource_watch;
          Alcotest.test_case "diagnostics HTTP server" `Quick
            test_diagnostics_server;
          Alcotest.test_case "bounded body" `Quick test_http_body_limit;
          Alcotest.test_case "request validation" `Quick
            test_http_request_validation;
        ] );
      ( "client",
        [
          Alcotest.test_case "paginated list" `Quick test_paginated_list;
          Alcotest.test_case "dynamic LIST TypeMeta defaulting" `Quick
            test_dynamic_list_type_meta_defaulting;
          Alcotest.test_case "collection subresources scale and logs" `Quick
            test_collection_subresources_scale_and_logs;
          Alcotest.test_case "log validation and limits" `Quick
            test_log_validation_and_limits;
          Alcotest.test_case "API error contract" `Quick test_api_error_contract;
          Alcotest.test_case "exact cached discovery mapper" `Quick
            test_discovery_mapper_exact;
          Alcotest.test_case "preferred cached discovery mapper" `Quick
            test_discovery_mapper_preferred;
          Alcotest.test_case "discovery mapper ambiguity" `Quick
            test_discovery_mapper_ambiguity;
          Alcotest.test_case "complete cached discovery mapper" `Quick
            test_discovery_mapper_all;
          Alcotest.test_case "concurrent cached discovery mapper" `Quick
            test_discovery_mapper_concurrent;
          Alcotest.test_case "Kubernetes Event recorder" `Quick
            test_event_recorder;
          Alcotest.test_case "watch bookmark and expiry" `Quick
            test_watch_bookmark_and_expiry;
          Alcotest.test_case "watch framing faults" `Quick
            test_watch_framing_faults;
          Alcotest.test_case "reflector resumes after truncation" `Quick
            test_reflector_resumes_after_truncated_watch;
          Alcotest.test_case "reflector compaction relist" `Quick
            test_reflector_compaction_relist;
          Alcotest.test_case "apply and finalizer requests" `Quick
            test_patch_and_finalizer_requests;
        ] );
      ( "auth",
        [
          Alcotest.test_case "exec credential refresh" `Quick
            test_exec_credential_refresh;
          Alcotest.test_case "exec credential contract" `Quick
            test_exec_credential_contract;
          Alcotest.test_case "relative exec credential" `Quick
            test_relative_exec_credential;
        ] );
    ]
