# Changelog

All notable changes are recorded here. The project follows Semantic Versioning.

## Unreleased

## 0.1.3 - 2026-09-09

### Fixed

- Build and run on native Win32 while retaining protected credential handling:
  require `secret` 0.1.3 for direct Win32 secret I/O, use a native monotonic
  clock, use selectable socket pairs for internal wakeups, and use the
  platform null device for exec credentials.

## 0.1.2 - 2026-09-07

### Fixed

- Require `ppxlib` 0.36.0 or newer so the `kube.ppx` deriver compiles when opam
  selects lower dependency bounds, and exercise lower-bound installs in project
  CI to prevent regressions.

## 0.1.1 - 2026-09-06

### Fixed

- Declare the minimum compatible `tls` and `yojson` versions so opam
  lower-bound builds select dependencies that provide the APIs used by kube.
- Skip loopback-server integration tests under opam-repository CI, whose build
  sandbox intentionally disallows binding local sockets. The tests continue to
  run normally in development and project CI.

## 0.1.0 - 2026-09-06

### Added

- `Http.Sensitive` and `Client.Sensitive` protected request paths backed by
  `Secret.t`, including scoped token-file authorization, bounded protected
  response bodies, direct protected JSON/base64 Secret creation and atomic
  patching, and protected ServiceAccount TokenRequest results. Existing string
  APIs remain source-compatible and retain their original memory semantics.

- A batteries-included `Kube.Operator` runner that centralizes standard CLI
  flags, configuration, signal cancellation, client ownership, diagnostics,
  health, metrics, and optional Lease leadership for examples and scaffolds.
- `Kube.Reconcile` helpers for typed Server-Side Apply and safe controller-owned
  children, plus cancellation-aware `Kube.Retry.on_conflict` optimistic retry.
- Kubernetes-standard custom-resource conditions with transition-preserving
  updates, strict codecs, and structural schemas.
- Compilation-checked common operator patterns, application instrumentation,
  an ecosystem parity audit, and a candid guide to choosing OCaml over Go or
  Rust.

- Native OCaml 5 Kubernetes HTTP and TLS transport.
- Authenticated WebSocket upgrades; versioned exec and attach channels with
  resize, half-close, and structured exit status; and multiplexed Pod
  port-forward streams with a local TCP forwarder.
- Kubeconfig, in-cluster, token-file, basic, client-certificate, and exec-plugin
  authentication.
- Typed and dynamic CRUD, patch, Server-Side Apply, pagination, discovery,
  status, arbitrary subresources, and watch support.
- Safe collection deletion with explicit cross-namespace opt-in; complete raw
  subresource POST, PUT, PATCH, DELETE, and streaming GET operations; typed
  `autoscaling/v1` Scale access; and bounded or streaming Pod logs across typed,
  dynamic, and cache-backed clients.
- Resource-version-aware reflectors, local stores, deduplicating work queues,
  delayed requeue, exponential retry, finalizer helpers, and concurrent
  controllers.
- Controller cache-sync readiness checks that cover primary and watched cache
  synchronization, initial queue seeding, worker startup, and shutdown.
- A typed custom-resource operator and repeatable kind integration test.
- Compatibility lanes for active Kubernetes minor releases.
- Checksum-pinned generated built-in API packages for Kubernetes 1.34 through
  1.37, with manifest-driven regeneration and offline drift checks.
- Kubeconfig merging and client-go-compatible exec-plugin validation, relative
  command resolution, and token-file precedence.
- Thread-safe persistent HTTP/1.1 connection pooling with stale-connection
  detection and explicit client shutdown.
- A configurable monotonic connection-establishment deadline covering isolated
  DNS lookup, nonblocking TCP connect, and interruptible TLS handshake.
- Independent monotonic request-write and response-header deadlines while
  leaving streaming response bodies under explicit cancellation control.
- Kubeconfig and programmatic user impersonation, including UID, repeated group,
  and percent-escaped extra headers with explicit per-request override behavior.
- Explicit kubeconfig HTTP forward/CONNECT proxies and authenticated SOCKS5
  tunnels with cancellation and connection-deadline coverage.
- Structured multi-controller supervision, shareable typed reflector caches,
  cache-sync barriers, predicates, arbitrary secondary watches, and typed
  owner-reference watches.
- Kubernetes Lease leader election with local observation-time expiry,
  optimistic conflict handling, per-attempt deadlines, jittered acquisition,
  renewal supervision, and release only after protected work is joined.
- Native Prometheus counters, gauges, and histograms; controller and leadership
  instrumentation; named liveness/readiness checks; and a supervised HTTP
  diagnostics component serving health, readiness, and metrics endpoints.
- Strict `admission.k8s.io/v1` codecs, typed resource handlers, validating and
  mutating outcomes, warnings, audit annotations, and complete RFC 6902 patch
  responses.
- Checked `apiextensions.k8s.io/v1` CRD conversion reviews with ordered mapping,
  desired-version validation, and name/namespace/UID identity preservation.
- A supervised TLS 1.2/1.3 webhook server with optional mutual TLS, bounded
  connections/headers/bodies, monotonic request deadlines, cancellation,
  readiness integration, per-path Prometheus metrics, and joined shutdown.
- A deterministic Kubernetes Swagger 2.0 compiler that computes transitive
  schema closures and emits strongly typed OCaml records, constructors, JSON
  codecs, common metadata helpers, and `Core.Resource` descriptors. Stable
  resource selection, plurals, and scopes are derived from upstream LIST/WATCH
  operations rather than maintained by hand.
- The `kube.api.v1_36` generated sublibrary, pinned to Kubernetes v1.36.2 and
  covering all 60 stable LIST/WATCH resources and 473 dependent definitions,
  with a machine-readable `all_resources` registry.
- Reproducible pruned OpenAPI input, generator golden tests, generated-codec
  round-trip tests, and the `@codegen-check` drift gate.
- Reconciliation requests carrying the controller lifetime token so in-flight
  client operations can be interrupted during structured shutdown.
- A default cache-synchronization deadline, optional per-reconciliation
  deadlines with linked child cancellation, prompt cancellation-aware sleeps,
  and timeout-labelled reconciliation metrics.
- Atomic, thread-safe secondary indexes on reflector stores and server-side
  field selectors for primary, secondary, and owner-reference watches.
- An explicit typed cache-backed client with synchronized GET, LIST, and index
  reads, classified cache misses, deliberate fresh-read methods, live mutation
  delegation, and automatic availability on every controller request.
- A thread-safe lazy discovery mapper for exact and preferred GVK/GVR
  resolution, plural/singular/short-name aliases, ambiguity detection,
  discovered scope, verbs and subresources, shared in-flight requests,
  generation-safe invalidation, and opt-in complete refresh.
- Configurable per-controller error policies with finite exponential backoff,
  retry budgets, and drop semantics.
- A cancellation-aware native token-bucket API-client rate limiter with shared,
  custom, and unlimited configurations.
- A finalizer lifecycle state machine that separates apply from cleanup and
  removes a finalizer only after successful cleanup.
- A deterministic scripted loopback API-server harness covering coalesced and
  byte-fragmented watch events, truncated chunk framing, HTTP and in-band 410
  recovery, reconnect continuity, and atomic compaction relists.
- The `kube.crd` sublibrary with a typed structural OpenAPI schema AST,
  validation, CRD names/versions/status/scale/printer-column support,
  deterministic JSON and YAML manifests, and a typed custom-resource functor.
- Typed custom-resource construction, strict spec/status decoding, status merge
  patch helpers, and a generated-manifest drift check used by the Greeting
  operator.
- The `kube.ppx` `[@@deriving kube]` rewriter for records, aliases, options,
  lists, arrays, string maps, structural tuples, nullary and payload variants,
  external derived types, and explicitly schema-bounded recursive types.
- Deriver attributes for stable field and constructor wire names, descriptions,
  and complete schema overrides, plus generated `.mli` declarations and
  compile-time duplicate-name diagnostics.
- Kubernetes-aware API error classifiers, structured retry-delay extraction
  from Status details and HTTP `Retry-After`, and server-directed reflector and
  leader-election backoff.
- An `events.k8s.io/v1` recorder, canonical ObjectReference serialization,
  UID-checked owner-reference constructors, proof-operator Event reporting, and
  a real-cluster Event assertion.
- A pluggable thread-safe structured logger with deterministic JSON output,
  immutable scoped fields, dynamic levels, explicit redaction, sink-failure
  isolation, sanitized HTTP request diagnostics, and lifecycle/error events
  across managers, reflectors, controllers, leader election, diagnostics, and
  webhook serving.
- A public injectable `Client.Transport` boundary and `kube.test` package that
  retain the production authentication, retry, request, decoding, watch, cache,
  and controller paths while providing deterministic responses, programmable
  handlers, concurrent request capture, streaming, cancellation, and strict
  script-completion checks.
- Explicit multi-namespace reflector and controller scopes with one independent
  LIST/WATCH resource version per namespace, aggregate readiness, namespace-local
  compaction relists, atomic scoped store replacement, and matching primary,
  secondary, and owner-watch plumbing.
- An installed `ocaml-kube scaffold` CLI that imports a selected served version
  from a v1 CRD and atomically generates a standalone typed operator project,
  controller/finalizer skeleton, package metadata, RBAC, deployment, sample, and
  container artifacts without overwriting an existing path.
- An `ocaml-kube init` type-first workflow whose starter OCaml models derive JSON
  codecs and structural schemas, generate the checked-in CRD manifest, expose a
  drift-check alias, and compile as an independent operator project.
- Schema-to-OCaml mapping for records, optional fields, primitive formats,
  arrays, typed maps, empty objects, Int-or-String, and string enums, with exact
  source schemas and path-specific raw-JSON diagnostics for unsupported
  structural shapes. `--deny-raw` turns those diagnostics into a generation
  failure.
- Generated operator entry points and deployments with optional Lease leader
  election, native metrics and probe endpoints, two-replica defaults, graceful
  supervision, and restricted non-root container security settings.
- A scaffold integration proof that generates and externally compiles a fresh
  project, runs it against kind, observes typed reconciliation and finalizer
  cleanup, and verifies graceful shutdown.
- An offline, pinned scaffold acceptance corpus covering Gateway API, KEDA, and
  Prometheus Operator CRDs, plus codec-only PPX derivation so imported schemas
  compile without recursively duplicating unused generated schemas.

### Security

- Bound buffered HTTP response bodies and individual watch frames.
- Reject newline injection in request targets and headers.
- Refresh cached exec credentials once after an HTTP 401 response.
- Validate HTTP header grammar, reserve framing headers to the transport, and
  reject unencoded request-target whitespace.

### Fixed

- Reliable SIGINT and SIGTERM cancellation while the operator supervisor is
  blocked waiting for components, including generated operators on Linux.
- Leader-election standby readiness now reports healthy while participating in
  election, preventing multi-replica rolling deployments from deadlocking while
  preserving the active-leader metric and single-reconciler execution.
- Recoverable per-connection port-forward stream failures, with bounded and
  diagnostic integration probes instead of indefinite CI hangs, and portable
  regression coverage for both clean EOF and connection-reset closure.
- Use a process-local monotonic clock for cancellation deadlines, controller
  latency, discovery cooldowns, queue scheduling, rate limiting, reflector
  recovery, exec-helper timeouts, diagnostics, and Lease expiry observation, so
  civil-clock adjustments cannot distort elapsed-time decisions.
- Preserve previous values for overlapping resources during an atomic relist,
  allowing secondary-watch and owner mappings to reconcile both the old and
  new relationships after resource-version compaction.
- Reconcile both the previous and current primary keys when a secondary watch
  changes relationships, including dependent-resource ownership transfers.
- Keep polling and draining exec credential helpers until both their process and
  output pipes complete, instead of returning after an initial nonblocking poll.
- Omit unknown resource versions from finalizer patches instead of sending JSON
  null.
- Decode JSON integer literals in common object metadata so generated int64
  generations remain visible to reconciliation logic.
- Retry transient initial LIST failures with jittered exponential backoff,
  protect against immediate clean-EOF watch reconnect loops, and propagate
  non-410 in-band watch Status failures into the reflector retry path.
- Apply watch size limits to individual JSON event frames rather than arbitrary
  transport chunks, so coalesced valid events are not rejected.
- Resume a reflector reconnect from the newest resource version already
  delivered to its cache after a transport failure.
- Classify socket EOF and framing failures as cancellation when an in-flight
  request's cancellation token shut down the connection.
- Fill missing TypeMeta from the requested API descriptor when decoding dynamic
  LIST/WATCH objects, matching API servers that omit `apiVersion` and `kind`
  inside collection items.
- Make the kind integration test recover stale finalizers left by interrupted
  runs and use group-qualified resource names when several CRDs share a kind.
