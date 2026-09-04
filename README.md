# ocaml-kube

A native OCaml 5 client and controller runtime for Kubernetes®.

> [!NOTE]
> Kubernetes® is a registered trademark of The Linux Foundation. This project
> is independent and is not affiliated with, sponsored by, or endorsed by The
> Linux Foundation or the Kubernetes project.

The project is pre-release and has not yet been published to OPAM.

## Highlights

- Typed clients for built-in resources and custom resources.
- Generated Kubernetes 1.34–1.37 API packages, plus discovery and dynamic APIs.
- Watches, reflectors, indexed caches, work queues, controllers, and leader
  election.
- Kubeconfig and in-cluster authentication, TLS, proxies, impersonation, and
  explicit cancellation.
- Pod logs, exec, attach, and port forwarding.
- CRD generation, admission and conversion webhooks, operator scaffolding, and
  a deterministic testkit.
- A batteries-included operator runner, declarative owned-child apply,
  conflict retries, standard status conditions, and compilation-checked common
  patterns.

## Build

OCaml 5.1 or newer and OPAM are required.

```sh
opam install . --deps-only --with-test --with-doc
opam exec -- dune build @all @install @doc @codegen-check
opam exec -- dune runtest
```

See [Getting started](docs/getting-started.md) for client setup, a typed request,
and the local integration example.

## Libraries

| Library | Purpose |
| --- | --- |
| `kube` | Configuration, transport, clients, discovery, caching, and controllers |
| `kube.api.v1_34` … `kube.api.v1_37` | Versioned generated Kubernetes APIs |
| `kube.crd` | Typed custom resources and CRD generation |
| `kube.ppx` | JSON codec and structural schema derivation |
| `kube.test` | Deterministic client and controller testing |

## Documentation

| Guide | Covers |
| --- | --- |
| [Getting started](docs/getting-started.md) | Build, configuration, and first request |
| [Client APIs](docs/client.md) | Typed, dynamic, subresource, and streaming clients |
| [Sensitive transport](docs/sensitive-transport.md) | Protected credentials, bodies, Secret writes, and TokenRequest |
| [Controller runtime](docs/controllers.md) | Reconciliation, caching, watches, and leadership |
| [Operator patterns](docs/operator-patterns.md) | Owned children, apply, retries, conditions, metrics, and tests |
| [Operations](docs/operations.md) | Logging, health, metrics, diagnostics, and events |
| [Compatibility](docs/compatibility.md) | Supported Kubernetes versions and feature floor |
| [Generated APIs](docs/api-codegen.md) | API packages, versioning, and regeneration |
| [Custom resources](docs/crds.md) | CRD schemas, codecs, and typed resources |
| [Operator scaffolding](docs/scaffolding.md) | OCaml-first generation and CRD import |
| [Streaming](docs/streaming.md) | Exec, attach, and port forwarding |
| [Webhooks](docs/webhooks.md) | Admission, conversion, and TLS serving |
| [Testing](docs/testing.md) | Scripted transports and integration boundaries |
| [Client-go and Rust comparison](docs/ecosystem-comparison.md) | Current parity, honest gaps, and priorities |
| [Why OCaml?](docs/why-ocaml.md) | Strengths, tradeoffs, and when to choose it |

Release history is in [CHANGES.md](CHANGES.md). See
[CONTRIBUTING.md](CONTRIBUTING.md), [SECURITY.md](SECURITY.md), and the
[release checklist](docs/releasing.md) for project processes.

Licensed under Apache-2.0.
