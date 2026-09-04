# Client-go and Rust comparison

The useful comparison is not whether every package has the same name. It is
whether an operator author has a safe, discoverable path for the recurring
control-loop jobs. This audit compares ocaml-kube with the official
[client-go packages](https://pkg.go.dev/k8s.io/client-go), Go's widely used
[controller-runtime helpers](https://pkg.go.dev/sigs.k8s.io/controller-runtime/pkg/controller/controllerutil),
and Rust's [kube runtime](https://docs.rs/kube/latest/kube/runtime/).

## Operator workflow

| Capability | ocaml-kube | Go ecosystem | Rust `kube` |
| --- | --- | --- | --- |
| Typed built-ins | Pinned 1.34–1.37 packages | Official generated clientset | `k8s-openapi` feature-selected schema |
| Dynamic discovery | Cached exact/preferred mapper | Discovery and dynamic clients | Dynamic `Api` and discovery |
| LIST/WATCH cache | Reflector, indexes, shared typed cache | Informers and listers | Watcher, reflector, `Store` |
| Level-triggered controller | Keyed dirty queue, workers, requeue | Workqueue/controller-runtime | `Controller` stream and `Action` |
| Related resources | `Watches` and `Owns` | Watches and owner enqueue | `watches` and `owns` |
| Finalizers | Checked apply/cleanup state machine | `controllerutil` helpers | Runtime `finalizer` helper |
| Declarative children | Typed `apply`/`apply_owned` with field manager | Server-Side Apply/client patch | `Patch::Apply` and `PatchParams::apply` |
| Optimistic retry | `Retry.on_conflict` | `retry.RetryOnConflict` | Application retry/error policy |
| Standard conditions | `Kube_crd.Condition.set` | `meta.SetStatusCondition` | `k8s-openapi` condition types |
| Process lifecycle | `Operator.run` | Manager/controller-runtime options | Usually composed with Tokio and a web server |
| Operations | JSON logs, Prometheus, probes, Events | Ecosystem-dependent; controller-runtime metrics/probes | `tracing` and Prometheus ecosystem |
| Webhooks | Typed admission/conversion and supervised TLS | Admission/controller-runtime webhooks | Application framework integration |
| Deterministic tests | Scripted/programmable transport | Fake clients/envtest | Mock service/test tower layers |
| New project | Type-first or CRD-import scaffold | Kubebuilder/operator-sdk | Templates and examples |

The Server-Side Apply behavior follows Kubernetes' field-management model: a
stable manager declares the fields the controller owns, while forced ownership
is an explicit choice. See the upstream
[Server-Side Apply reference](https://kubernetes.io/docs/reference/using-api/server-side-apply/).

## Gaps that remain

The current implementation covers the operator control loop well, but parity is
not the same as ecosystem maturity:

- client-go is upstream, updates first, and has the largest body of production
  integrations and troubleshooting knowledge. ocaml-kube intentionally pins
  generated schemas, so a new Kubernetes minor requires an explicit package
  update.
- Strategic merge patch for built-in resources is not implemented. Prefer
  Server-Side Apply for new controllers; use JSON or merge patch where apply is
  inappropriate.
- Metadata-only reflectors and cache transforms are not yet available. Large
  Pod caches therefore retain full decoded objects instead of offering the
  memory reductions available in kube-rs.
- Metrics, structured logging, and Events are native, but there is no built-in
  OpenTelemetry trace exporter or reconcile-span propagation yet.
- The runtime uses OCaml 5 system threads and explicit cancellation. It does
  not currently provide Lwt or Eio transport adapters.
- Imported CRD schemas deliberately fall back to typed raw JSON for structural
  unions, embedded resources, and preserved-unknown sections that cannot yet be
  represented losslessly.
- A small community cannot match the breadth of Go and Rust examples today.
  This repository therefore treats compilation-checked patterns and generated
  projects as part of the API, not incidental documentation.

## Priorities

The next parity work should be driven by operator outcomes:

1. metadata-only watches and cache transforms for memory-sensitive clusters;
2. OpenTelemetry spans that carry object key, controller, attempt, and result;
3. a wait/condition API for rollout and readiness workflows;
4. richer fake-server state for create/update/apply assertions while keeping
   real API-server semantics in kind; and
5. schema support for more Kubernetes unions and embedded-resource shapes.

The comparison is intentionally candid: use the OCaml client for the strengths
it has now, and keep these boundaries visible when choosing it for production.
