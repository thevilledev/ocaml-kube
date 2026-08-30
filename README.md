# ocaml-k8s

`ocaml-k8s` is a Kubernetes API client and controller runtime built directly on
OCaml 5, system threads, Unix sockets, and explicit cancellation. Kubernetes
machinery is implemented in this repository; established OCaml libraries are
used only for cryptography, TLS, URI handling, and JSON.

The current pre-release implementation includes:

- kubeconfig and in-cluster service-account authentication, including exec
  credential plugins, token refresh, basic auth, and client certificates;
- custom or system CAs and TLS hostname/IP verification;
- a streaming HTTP/1.1 transport with chunked transfer decoding, cancellation,
  injection checks, and bounded buffered responses;
- typed GET, LIST, CREATE, UPDATE, DELETE, JSON Patch, Merge Patch, and
  Server-Side Apply requests;
- status and arbitrary subresources, paginated consistent lists, watches,
  bookmarks, optional streaming initial events, and explicit 410
  resource-version recovery;
- runtime discovery and dynamic access to arbitrary resources;
- a thread-safe reflector store, dirty-key deduplicating work queue, delayed
  scheduling, per-key exponential retry, and concurrent reconcile workers;
- list/watch recovery, finalizer helpers, and graceful shutdown; and
- a typed `Greeting` CRD operator used by the kind integration proof.

The compatibility policy covers active Kubernetes minors. CI uses real 1.34,
1.35, and 1.36 API servers, with a source-built 1.37 edge lane until an official
kind image is available. See [the compatibility matrix](docs/compatibility.md).
No release has been published yet. Generated built-in API modules, persistent
connection pooling, metrics, leader election, and deeper watch fuzzing remain
pre-1.0 work.

## Build and test

Use the repository's opam switch environment:

```sh
opam exec -- dune build @all @install @doc
opam exec -- dune runtest
```

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
kubectl --kubeconfig "$PWD/kubeconfig.kind" get greeting hello-ocaml \
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
      (* Compare desired and observed state, then use [Api] to update it. *)
      Ok (Kube.Controller.Requeue_after 30.)

let run cancel client =
  Controller.run ~cancel ~workers:4 client ~reconcile
```

Keys, rather than entire event histories, drive reconciliation. This makes the
queue level-triggered: changes that arrive during reconciliation mark the key
dirty and cause exactly one subsequent pass, without allowing the same object to
run concurrently.

## Layout

- `Core` defines resource identity, metadata, paths, scope, and resource versions.
- `Config`, `Http`, and `Client` implement authentication and Kubernetes API calls.
- `Discovery` and `Dynamic` cover APIs not known at compile time.
- `Store`, `Work_queue`, and `Controller` implement the controller machinery.
- `examples/greeting_operator.ml` is the end-to-end typed operator.
- `deploy/` contains its structural CRD schema, sample, and least-privilege RBAC.

Release history is maintained in [CHANGES.md](CHANGES.md). Development and
security processes are documented in [CONTRIBUTING.md](CONTRIBUTING.md) and
[SECURITY.md](SECURITY.md). Maintainers should follow the explicit
[release checklist](docs/releasing.md); CI never publishes artifacts.

Licensed under Apache-2.0.
