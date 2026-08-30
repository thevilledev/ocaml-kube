# Contributing

Contributions are welcome. Until the first release, public APIs can still change,
but changes should preserve Kubernetes protocol correctness and include tests.

## Development setup

Use OCaml 5.1 or newer and install the package's development dependencies:

```sh
opam install . --deps-only --with-test --with-doc
opam exec -- dune build @all @install @doc
opam exec -- dune runtest
```

Format code with `opam exec -- dune fmt` and run `opam lint kube.opam` before
submitting a change.

## Kubernetes integration tests

Create a kind cluster and write its kubeconfig to `kubeconfig.kind`, then run:

```sh
test/integration_kind.sh
```

Changes to watches, authentication, resource versions, patching, finalizers, or
the controller queue should include a focused regression test as well as the
integration run.

Use the public `kube.test` transport for request-pipeline and reconciler unit
tests that do not require API-server semantics. Keep the loopback HTTP harness
for wire framing, socket, and connection-pool tests. A test whose result depends
on Kubernetes defaulting, validation, admission, storage, or resource-version
behavior belongs in the kind integration suite.

## Design principles

- Keep Kubernetes machinery native to this repository.
- Prefer explicit state, cancellation, and ownership over ambient runtime state.
- Treat LIST and WATCH semantics as correctness-critical.
- Preserve unknown JSON fields in dynamic resources.
- Avoid claiming version compatibility without a real API-server test.
- Keep public interfaces small and document behavioral guarantees.
