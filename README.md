# ocaml-k8s

`ocaml-k8s` is a Kubernetes API client and controller runtime built directly on
OCaml 5, system threads, Unix sockets, and explicit cancellation. Kubernetes
machinery is implemented in this repository; established OCaml libraries are
used only for cryptography, TLS, URI handling, JSON, and standard compression.

The current pre-release implementation includes:

- kubeconfig and in-cluster service-account authentication, including exec
  credential plugins, token refresh, basic auth, client certificates, user
  impersonation, and explicit HTTP or SOCKS5 proxies;
- custom or system CAs and TLS hostname/IP verification;
- a streaming HTTP/1.1 transport with persistent connection pooling, chunked
  transfer decoding, monotonic connection, request-write, and response-header
  deadlines, cancellation, injection checks, and bounded buffered responses,
  plus a cancellation-aware token-bucket client rate limiter;
- native WebSocket framing and Kubernetes exec/attach stream protocols with
  terminal resize, stdin half-close, structured exit status, and cancellation;
- Kubernetes WebSocket/SPDY port forwarding with bounded multiplexed streams
  and a supervised multi-port local TCP forwarder;
- typed GET, LIST, CREATE, UPDATE, DELETE, collection deletion, JSON Patch,
  Merge Patch, and Server-Side Apply requests;
- Kubernetes Status error classifiers and server-directed retry delays from
  both Status details and HTTP `Retry-After` headers;
- complete generic subresource reads and writes, typed status and Scale
  operations, bounded and streaming Pod logs, paginated consistent lists,
  watches, bookmarks, optional streaming initial events, and explicit 410
  resource-version recovery;
- runtime discovery and dynamic access to arbitrary resources, including a
  thread-safe lazy mapper for exact and preferred GVK/GVR resolution, resource
  aliases, explicit invalidation, and discovered subresources;
- shared typed reflectors and local stores with atomic secondary indexes,
  all-namespace, single-namespace, or independent multi-namespace streams, and
  server-side label/field selectors; explicit typed cache-backed GET, LIST, and
  index reads paired with live writes; dirty-key deduplicating work queues,
  delayed scheduling, configurable retry policies and retry budgets, and
  concurrent reconcile workers;
- a structured controller manager with dependency deduplication, cache-sync
  barriers and deadlines, optional per-reconciliation deadlines, typed
  secondary watches, owner-reference watches, and predicates;
- Lease-based leader election with local-time expiry observation, bounded API
  attempts, optimistic resource-version updates, and join-before-release
  shutdown;
- a thread-safe Prometheus counter, gauge, and histogram registry with built-in
  controller and leadership metrics; and
- pluggable thread-safe structured logging with scoped component fields,
  runtime level changes, redacted values, and isolated sink failures; and
- a supervised native HTTP diagnostics server for `/healthz`, `/readyz`, and
  `/metrics` with bounded request headers and graceful shutdown;
- typed validating and mutating admission handlers, RFC 6902 responses, CRD
  conversion review validation, and a bounded supervised TLS webhook server
  with optional client-certificate authentication;
- list/watch recovery, a finalizer lifecycle state machine, and graceful
  shutdown; and
- `events.k8s.io/v1` recording with canonical object-reference and UID-checked
  owner-reference helpers; and
- independently selectable Kubernetes v1.34, v1.35, v1.36, and v1.37 API
  packages generated from checksum-pinned official OpenAPI specifications,
  plus matrix regeneration and offline drift automation; and
- a `kube.crd` library for typed structural schemas, validated CRD manifests,
  deterministic YAML, typed custom-resource codecs, and status patch helpers;
  and
- a `kube.ppx` deriver that generates JSON codecs and structural schemas from
  records, variants, aliases, collections, maps, tuples, and explicitly bounded
  recursive types; and
- a public `kube.test` package with an injectable client transport, deterministic
  FIFO responses, programmable concurrent handlers, request capture, body-bound
  and streaming behavior, and cancellation-aware long-running watches; and
- a typed `Greeting` operator whose resource type and drift-checked CRD manifest
  share the same definition and are exercised by the kind integration proof;
  and
- installed `ocaml-k8s init` and `ocaml-k8s scaffold` commands that emit
  independent, typed, buildable operator projects either from OCaml-first model
  definitions or by importing an existing v1 CRD, with controller, finalizer,
  RBAC, deployment, sample, and container artifacts.

The compatibility policy covers active Kubernetes minors. CI uses real 1.34,
1.35, and 1.36 API servers, with a source-built 1.37 edge lane until an official
kind image is available. See [the compatibility matrix](docs/compatibility.md).
No release has been published yet. Richer schema/CEL constraints, automatic
webhook certificate rotation, and deeper watch fuzzing remain pre-1.0 work.

## Build and test

Use the repository's opam switch environment:

```sh
opam exec -- dune build @all @install @doc
opam exec -- dune runtest
opam exec -- dune build @codegen-check
```

## Connection configuration

`Config.load_default` selects in-cluster credentials when the service-account
environment is present and otherwise merges `KUBECONFIG` or the standard user
configuration. The kubeconfig `proxy-url`, `as`, `as-uid`, `as-groups`, and
`as-user-extra` fields are honored. HTTP proxies use absolute-form requests for
plain API endpoints and CONNECT tunnels for TLS endpoints; SOCKS5 supports both
anonymous and username/password handshakes. TLS-encrypted proxy endpoints are
rejected explicitly because nested TLS is not yet implemented.

Programmatic clients use validated configuration values and independent
transport phase deadlines:

```ocaml
let impersonation =
  Kube.Config.make_impersonation ~groups:[ "developers" ] ~user:"alice" ()
  |> Result.get_ok

let config =
  Kube.Config.make ~impersonation
    ~proxy_url:(Uri.of_string "socks5://proxy.internal:1080")
    (Uri.of_string "https://kubernetes.default.svc")

let client =
  Kube.Client.create ~connect_timeout:10. ~write_timeout:30.
    ~response_header_timeout:30. config
```

The response-header deadline ends once a complete HTTP header block arrives.
Response bodies deliberately have no wall-clock deadline because watches and log
streams can remain open indefinitely; their cancellation token controls their
lifetime.

## Start a type-first operator

```sh
ocaml-k8s init \
  --output widget-operator \
  --group example.dev \
  --kind Widget
cd widget-operator
opam install . --deps-only
opam exec -- dune build @all @codegen-check
```

The generated `Spec`, `Status`, and `Phase` OCaml types derive their JSON codecs
and structural OpenAPI schema together. Editing those types and running
`dune exec tools/generate_crd.exe > deploy/crd.yaml` updates the manifest;
`@codegen-check` rejects drift. Identity flags provide version, plural,
singular, scope, and package-name overrides.

## Import an operator from a CRD

```sh
ocaml-k8s scaffold --crd deploy/widget-crd.yaml --output widget-operator
cd widget-operator
opam install . --deps-only
opam exec -- dune build @all
```

The command selects the unique storage version unless `--version` is supplied,
generates typed spec/status models for structural schema shapes, and uses an
explicit, path-diagnosed raw-JSON fallback for unsupported subtrees. Use
`--deny-raw` to make any such fallback fail generation. It refuses to overwrite
an existing path. The generated deployment runs two Lease-coordinated replicas
with probes, metrics, and a restricted container security context. See
[operator scaffolding](docs/scaffolding.md) for both workflows, the generated
tree, schema mapping, input constraints, and end-to-end kind proof.

## Generated Kubernetes API

The separately versioned `kube.api.v1_34`, `kube.api.v1_35`,
`kube.api.v1_36`, and `kube.api.v1_37` sublibraries provide generated records,
constructors, JSON codecs, and `Core.Resource` descriptors for every stable
LIST/WATCH resource in each pinned upstream schema. Choose the schema line that
matches the API surface your program is compiled against. For example:

```ocaml
module K8s = Kube_api_v1_36

let metadata =
  K8s.Meta_v1.ObjectMeta.make ~name:"settings" ~namespace:"default" ()

let config_map =
  K8s.Core_v1.ConfigMap.make ~api_version:"v1" ~kind:"ConfigMap"
    ~metadata ~data:[ ("mode", "production") ] ()

module Config_maps = Kube.Client.For (K8s.Core_v1.ConfigMap)
```

The checked-in dependency-closure schema makes normal builds and drift checks
self-contained. Maintainer regeneration verifies the complete upstream document
against its pinned checksum, derives the resource selection from its operations
and response schemas, creates new versioned package directories, and only then
replaces generated files. See
[generated API versioning and regeneration](docs/api-codegen.md).

## Discovery and dynamic APIs

Exact discovery resolves a Kubernetes group/version/kind or
group/version/resource into a `Core.api` descriptor suitable for dynamic calls:

```ocaml
let mapper = Kube.Discovery.Mapper.create client

let deployment =
  Kube.Discovery.Mapper.resolve_gvk mapper ~group:"apps" ~version:"v1"
    ~kind:"Deployment"

let preferred_deployment =
  Kube.Discovery.Mapper.resolve_resource ~group:"apps" mapper
    ~resource:"deploy"

let get_dynamic name =
  match deployment with
  | Error error -> Error error
  | Ok mapping ->
      Kube.Dynamic.get client ~api:mapping.api ~namespace:"default" name
```

An exact lookup fetches and caches only its requested group-version. Preferred
lookups use the server-advertised group version and resolve plural, singular, or
short resource names. An unqualified preferred lookup rejects collisions across
API groups rather than guessing. Concurrent callers share the same in-flight
request, and the resulting mapping includes verbs, aliases, categories, scope,
and subresources. `mappings` performs explicit complete discovery; `invalidate`
and `refresh` control cache lifetime.

## Subresources, Scale, and logs

Generic subresources use raw JSON because Kubernetes defines many distinct
request and response protocols on them. POST, PUT, PATCH, DELETE, and streaming
GET operations are available on every typed resource client and through
`Kube.Dynamic`. This covers ordinary extension endpoints such as eviction while
keeping protocol-specific construction explicit.

Scale uses the common `autoscaling/v1` representation:

```ocaml
module Deployments = Kube.Client.For (K8s.Apps_v1.Deployment)

let set_replicas client name replicas =
  match Deployments.get_scale client ~namespace:"default" name with
  | Error _ as error -> error
  | Ok scale ->
      let scale =
        { scale with spec = { Kube.Client.replicas = Int32.of_int replicas } }
      in
      Deployments.replace_scale client ~namespace:"default" name scale
```

Pod logs can be buffered with an explicit size bound or consumed as arbitrary
transport chunks. A continuing stream is stopped through the normal cancellation
token:

```ocaml
module Pods = Kube.Client.For (K8s.Core_v1.Pod)

let follow cancel client name =
  let options =
    { Kube.Client.default_log_options with container = Some "operator"; follow = true }
  in
Pods.stream_logs ~cancel ~options client ~namespace:"default" name
    ~on_chunk:(output_string stdout)
```

## Remote commands and port forwarding

`Remote_command` implements the versioned Kubernetes exec/attach channel
protocols, including terminal resize, stdin half-close, and structured exit
statuses. `Port_forward` multiplexes paired error/data streams, while
`Port_forward.Forwarder` binds local TCP listeners:

```ocaml
let exec =
  Kube.Remote_command.exec ~stdin:true client ~pod:"worker"
    ~command:[ "sh"; "-c"; "make migrate" ] ()

let mapping =
  Kube.Port_forward.Forwarder.{ local_port = 8080; remote_port = 8080 }

let forwarding =
  Kube.Port_forward.Forwarder.start client ~pod:"web" ~ports:[ mapping ] ()
```

Both APIs use the same kubeconfig, authentication, TLS, proxy, impersonation,
rate-limit, cancellation, and shutdown behavior as ordinary client requests.
See [streaming subresources](docs/streaming.md) for the complete session APIs
and operational limits.

Namespaced collection deletion deliberately defaults to the configured or
`default` namespace. Deleting through the cross-namespace collection endpoint
requires `~all_namespaces:true`; it cannot be combined with `~namespace`.

## Typed custom resources

`kube.ppx` derives the JSON codec and structural schema together. Add
`(preprocess (pps kube.ppx))` to the consuming Dune stanza, then pass the derived
spec and status modules to the custom-resource functor:

```ocaml
module Spec = struct
  type t = {
    message : string
      [@kube.schema Kube_crd.Schema.string ~min_length:1 ()];
  }
  [@@deriving kube]
end

module Phase = struct
  type t = Pending | Ready | Failed of string [@@deriving kube]
end

module Status = struct
  type t = { observed_generation : int; phase : Phase.t }
  [@@deriving kube]
end

module Greeting = Kube_crd.Resource.Make (struct
  module Spec = Spec
  module Status = Status
  let group = "demo.example.com"
  let version = "v1alpha1"
  let kind = "Greeting"
  let plural = "greetings"
  let singular = "greeting"
  let scope = Kube.Core.Namespaced
  let short_names = [ "greet" ]
  let categories = []
end)

module Greetings = Kube.Client.For (Greeting)
```

`Kube_crd.Custom_resource_definition.to_yaml Greeting.crd` emits the installable
manifest. The proof operator uses exactly this path, and `@codegen-check` fails
if its checked-in manifest drifts. See
[custom resources and CRD generation](docs/crds.md).

## Admission and conversion webhooks

`Admission.For(Resource)` decodes typed current and previous objects and
produces validating or JSON Patch mutation responses. `Conversion.map` handles
ordered CRD version conversion and checks output identity. Both protocols run
on the bounded TLS server and compose with `Manager`, `Health`, and `Metrics`:

```ocaml
module Greeting_admission = Kube.Admission.For (Greeting)

let validate =
  Greeting_admission.handler (fun ~cancel:_ request ->
      match request.object_ with
      | Some greeting when String.trim greeting.spec.message <> "" ->
          Ok (Kube.Admission.allow ())
      | _ ->
          Ok
            (Kube.Admission.deny ~code:422 ~reason:"Invalid"
               "spec.message must not be empty"))

let webhook =
  Kube.Webhook.create ~address:"0.0.0.0" ~certificate_pem ~private_key_pem ()
  |> Result.get_ok

let () =
  Kube.Webhook.add_admission webhook ~path:"/validate-greeting" validate;
  Kube.Manager.add manager (Kube.Webhook.component webhook)
```

See [admission and conversion webhooks](docs/webhooks.md) for mutation,
conversion, mutual TLS, readiness, metrics, cancellation, and certificate
lifecycle guidance.

## Run the proof operator

Create a local cluster and keep its kubeconfig out of source control:

```sh
kind create cluster \
  --name ocaml-k8s-poc \
  --kubeconfig "$PWD/kubeconfig.kind"
kubectl --kubeconfig "$PWD/kubeconfig.kind" apply -f deploy/crd.yaml
opam exec -- dune exec examples/greeting_operator.exe -- \
  --kubeconfig "$PWD/kubeconfig.kind"
```

In another terminal, create and inspect the resource:

```sh
kubectl --kubeconfig "$PWD/kubeconfig.kind" apply -f deploy/sample.yaml
kubectl --kubeconfig "$PWD/kubeconfig.kind" \
  get greetings.demo.ocaml-k8s.dev hello-ocaml \
  -o jsonpath='{.status.reconciledMessage}{"\n"}'
```

Changing `spec.message` updates the status through the `/status` subresource.
Deleting the resource exercises finalization before it disappears.

Once the cluster exists, the complete assertion-driven proof can be repeated with:

```sh
test/integration_kind.sh
```

## Controller API

A resource module supplies its API identity, metadata accessor, and JSON codecs.
The controller functor then provides a typed request:

```ocaml
module Api = Kube.Client.For (Greeting)
module Controller = Kube.Controller.Make (Greeting)

let reconcile client (request : Controller.request) =
  match request.resource with
  | None -> Ok Kube.Controller.Done
  | Some greeting ->
      let _same_snapshot =
        Controller.Cached.get_by_key request.reader request.key
      in
      (* Pass [request.cancel] to every blocking client operation. *)
      Ok (Kube.Controller.Requeue_after 30.)

let run cancel client =
  Controller.run ~cancel ~workers:4 client ~reconcile
```

For multiple controllers, the manager owns one cancellation scope and
deduplicates shared reflector dependencies:

```ocaml
let cache = Controller.Cache.create ()

let component =
  Controller.component ~cache ~name:"greeting-status" ~workers:4 ~reconcile ()

let run cancel client =
  let manager = Kube.Manager.create ~cancel client in
  Kube.Manager.add manager component;
  Kube.Manager.run manager
```

Namespaced controllers use one all-namespace LIST/WATCH stream by default.
Select one namespace with `~namespace`, or a deliberate set with
`~namespaces`. Each selected namespace has an independent resource version and
recovery loop, while all objects remain in one typed cache:

```ocaml
let cache = Controller.Cache.create ~namespaces:[ "team-a"; "team-b" ] ()

let component =
  Controller.component ~cache ~name:"greeting-status" ~workers:4 ~reconcile ()
```

The cache becomes ready only after both initial snapshots are installed. A 410
or reconnect in one namespace cannot discard objects from another. The same
scope options are available to `Watches` and `Owns`; a supplied cache already
owns its scope and selectors. See [cache scope and recovery](docs/caching.md).

`Controller.Watches(Resource)` maps typed secondary-resource events to primary
keys. `Controller.Owns(Resource)` supplies the standard mapping from Kubernetes
controller owner references. All primary and secondary caches complete their
initial consistent LIST before reconciliation workers start. Passing the same
typed cache to several components results in one LIST/WATCH stream.

Each request also carries `request.reader`, a `Controller.Cached.t` backed by the
already-synchronized primary cache. `Controller.Cached.get`, `list`, and
`by_index` do not issue HTTP requests. Cache misses become ordinary classified
Kubernetes `NotFound` errors; an unsynchronized cache is rejected. There is no
silent live fallback because a namespace- or selector-filtered cache cannot
prove that an absent object does not exist. Use `fresh_get`/`fresh_list_all` for
an intentional API-server read. `create`, `replace`, `delete`, `patch`, status,
and subresource methods always write directly to the API server.

High-availability replicas can scope the manager to Kubernetes Lease
leadership. The leadership token is cancelled and the manager is fully joined
before a graceful Lease release:

```ocaml
let election =
  Kube.Leader_election.default ~namespace:"default"
    ~name:"greeting-operator" ~identity:pod_identity

Kube.Leader_election.run ~cancel client election (fun leadership_cancel ->
    let manager = Kube.Manager.create ~cancel:leadership_cancel client in
    Kube.Manager.add manager component;
    Kube.Manager.run manager)
```

Every replica must use a unique identity. Lease election provides the standard
Kubernetes single-active-controller convention, not a fencing guarantee for
external systems.

Diagnostics are another manager component, so a bind or serving failure cancels
the same structured scope as controllers:

```ocaml
let health = Kube.Health.create ()
let metrics = Kube.Metrics.create ()
let diagnostics =
  Kube.Diagnostics.create ~address:"0.0.0.0" ~port:8080 ~health ~metrics ()

Kube.Manager.add manager (Kube.Diagnostics.component diagnostics)
```

## Testing clients and controllers

`kube.test` replaces only the final network exchange. Authentication, rate
limiting, request construction, status classification, decoding, watch framing,
reflectors, and controllers continue through their production implementations:

```ocaml
module Test = Kube_test.Transport

let transport =
  Test.scripted
    [ Test.respond_json (`Assoc [ ("kind", `String "APIVersions") ]) ]

let client = Kube_test.client transport
let response = Kube.Client.raw client `GET "/api"

let () = Result.get_ok (Test.verify_complete transport)
```

Programmable handlers can route by method and target and can keep watch streams
open until cancellation. Captured requests contain resolved authentication
headers and serialized bodies for assertions. Use this package for fast,
deterministic unit tests; use a real API server for admission, defaulting,
resource-version, and integration behavior. See [testing clients and
controllers](docs/testing.md).

## Structured logging

Library logging is disabled by default. Supply one logger to the client and it
is inherited by reflectors, controllers, managers, leader election, diagnostics,
and webhook servers:

```ocaml
let logger = Kube.Log.stderr ~min_level:Kube.Log.Info ()
let client = Kube.Client.create ~logger config
```

`Log.stderr` emits one JSON object per line. Custom sinks receive typed events;
sink calls are serialized, and a failing sink is isolated from controller work
and counted by `Log.dropped_events`. Derived loggers add immutable context with
`Log.with_name` and `Log.with_fields`, while `Log.set_min_level` changes the
threshold shared by every derivative.

HTTP request events are debug-level and deliberately contain only a request ID,
method, path, duration, result class, and status when available. Query strings,
headers, authentication material, request bodies, response bodies, and API error
messages are not recorded by that layer. Application fields that must remain
opaque can use `Log.Redacted`.

Controller components accept `~metrics` to publish reconciliation counts,
latency histograms, and active-worker gauges. The proof operator additionally
reports leadership state and makes standby replicas fail readiness while
remaining live.

Keys, rather than entire event histories, drive reconciliation. This makes the
queue level-triggered: changes that arrive during reconciliation mark the key
dirty and cause exactly one subsequent pass, without allowing the same object to
run concurrently. Each request carries the controller cancellation token;
reconcilers pass it to blocking client calls so manager or leadership shutdown
can interrupt in-flight API work.

Controller startup bounds cache synchronization to 120 seconds by default.
`~cache_sync_timeout` changes that bound, while `~reconcile_timeout` gives each
reconciliation a linked child token and reports deadline expiry through the
normal error policy. These deadlines remain cooperative: code performing
blocking work must pass `request.cancel`, and CPU-bound code must check it.
Passing the diagnostics registry as `~health` registers a controller-specific
cache-sync readiness check. It becomes ready only after every cache is
synchronized, initial keys are seeded, and workers are running, and becomes
unready again as soon as shutdown begins.

`Controller.Finalizer(Resource).run` wraps normal and deletion reconciliation
in a small state machine. It persists the finalizer before calling `Apply`,
calls `Cleanup` for deleting objects, and removes the finalizer only after
cleanup returns `Done`. Cache secondary indexes are updated atomically with the
objects they index, so reconcilers can efficiently map relationships that are
not represented by owner references.

Reconcilers can publish supplemental Kubernetes Events without depending on the
generated built-in API package:

```ocaml
let recorder =
  Kube.Events.create ~client
    ~reporting_controller:"demo.example.com/foo-controller"
    ~reporting_instance:pod_uid ()
  |> Result.get_ok

let regarding = Kube.Core.object_reference Foo.api (Foo.metadata foo) in
Kube.Events.record recorder ~regarding ~type_:Kube.Events.Normal
  ~reason:"Reconciled" ~action:"UpdateStatus" ~note:"Foo is ready"
```

Events are intentionally best-effort diagnostics; a controller should report a
publication failure but must not confuse it with failure of the operation the
Event describes.

## Layout

- `Core` defines resource identity, metadata, paths, scope, and resource versions.
- `Config`, `Http`, `Websocket`, `Client`, and `Cached_client` implement
  authentication, live API calls, typed Scale and log streaming, upgraded
  connections, and explicit cache-backed reads.
- `Remote_command` implements exec/attach; `Port_forward` provides the
  multiplexed Pod tunnel and local TCP forwarder.
- `Discovery` and `Dynamic` cover APIs not known at compile time.
- `Store`, `Reflector`, `Work_queue`, `Manager`, and `Controller` implement the
  controller machinery.
- `Leader_election`, `Metrics`, `Health`, and `Diagnostics` provide the
  production operations surface; `Events` records Kubernetes Events.
- `Admission`, `Conversion`, and `Webhook` provide typed admission, checked CRD
  conversion, and supervised HTTPS serving.
- `Kube_api_v1_34` through `Kube_api_v1_37` are generated, independently
  selectable stable built-in API surfaces.
- `Kube_crd` defines structural schemas, CRD manifests, deterministic YAML,
  and the typed custom-resource functor.
- `Kube_ppx` implements `[@@deriving kube]` for codecs and schemas.
- `Kube_test` provides the injectable deterministic client and controller
  testkit installed as `kube.test`.
- `codegen/` contains the OpenAPI compiler, pinned selection manifest,
  reproducible schema closure, and golden tests.
- `scaffold/` contains the CRD importer, typed project generator, installed CLI,
  fixture, and generator tests.
- `examples/greeting_operator.ml` is the end-to-end typed operator.
- `deploy/` contains its structural CRD schema, sample, and least-privilege RBAC.

Release history is maintained in [CHANGES.md](CHANGES.md). Development and
security processes are documented in [CONTRIBUTING.md](CONTRIBUTING.md) and
[SECURITY.md](SECURITY.md). Maintainers should follow the explicit
[release checklist](docs/releasing.md); CI never publishes artifacts.

Licensed under Apache-2.0.
