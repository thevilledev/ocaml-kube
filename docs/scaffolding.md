# Operator scaffolding

The installed `ocaml-k8s` executable supports two starting points. `init` makes
OCaml types authoritative for a new API, while `scaffold` imports an existing
CRD and preserves its schema as authoritative.

## Start from OCaml types

```sh
ocaml-k8s init \
  --output widget-operator \
  --group example.dev \
  --kind Widget
```

The starter defines typed `Spec`, `Status`, and `Phase` modules with
`[@@deriving kube]`. The deriver produces strict JSON codecs and the structural
OpenAPI schema used by `Kube_crd.Resource.Make`; that resource module in turn
provides typed API operations, status helpers, and the validated CRD value.
Defaults are `v1alpha1`, namespaced scope, a kebab-cased singular derived from
Kind, its `s`-suffixed plural, and `<kind>-operator` as the package name. Override
them with `--version`, `--scope cluster`, `--singular`, `--plural`, or `--name`.

The checked-in manifest is generated from the compiled model:

```sh
opam exec -- dune exec tools/generate_crd.exe > deploy/crd.yaml
opam exec -- dune build @codegen-check
```

The drift gate is clean in a newly initialized project and fails after a model
change until the manifest is regenerated. This makes one OCaml definition the
source of the codecs, schema, CRD YAML, typed client, and status operations.

## Import an existing CRD

The installed `ocaml-k8s` executable turns one
`apiextensions.k8s.io/v1` CustomResourceDefinition into an independent OCaml
operator project:

```sh
ocaml-k8s scaffold \
  --crd deploy/widgets.example.dev.yaml \
  --output widget-operator
```

The command selects the CRD's unique storage version. Use `--version v1beta1`
to select another served version and `--name my-operator` to override the
package and executable name. The output path and the generator's temporary
sibling path must not already exist; scaffolding never merges into or overwrites
an existing tree. Project names are Kubernetes label-compatible and therefore
limited to 63 lowercase alphanumeric or hyphen characters.

Every schema subtree that cannot be represented precisely is reported with its
exact CRD path and a reason. Add `--deny-raw` in CI to reject such fallbacks
instead of generating them:

```sh
ocaml-k8s scaffold --deny-raw --crd deploy/widgets.example.dev.yaml \
  --output widget-operator
```

## Generated project

The output contains:

- a Dune project and opam package;
- typed `Spec` and `Status` records, aliases, arrays, maps, and string enums;
- JSON codecs and the exact selected CRD schema subtrees;
- a `Kube.Core.Resource` implementation and typed API client;
- a runnable reflector-backed controller with cancellation, finalizer lifecycle,
  optional Lease leader election, metrics, liveness, and readiness endpoints;
- the original CRD, least-privilege starting RBAC, Deployment, and sample
  manifests; and
- a multi-stage container build.

Build and run it with the same commands as an ordinary OCaml project:

```sh
cd widget-operator
opam install . --deps-only
opam exec -- dune build @all
opam exec -- dune exec widget-operator -- --kubeconfig ~/.kube/config
```

The generated reconciler deliberately performs no application-specific work.
Its `Apply` and `Cleanup` branches identify the two required implementation
boundaries. The finalizer is installed before `Apply`, and it is removed only
after `Cleanup` succeeds.

A direct local run keeps leader election and the diagnostics listener disabled.
The generated Deployment enables Lease election for two replicas, binds the
diagnostics server on port 8080, configures `/healthz` and `/readyz` probes, and
uses a non-root, read-only-root-filesystem container security context. Readiness
stays false until leadership is acquired and every controller cache has
synchronized and started its workers; it returns to false during shutdown.

## Schema mapping

The first release maps structural schema shapes as follows:

| Kubernetes schema | OCaml representation |
| --- | --- |
| object properties | record |
| optional property | `option` |
| string enum | variant with explicit wire names |
| string, boolean, number | `string`, `bool`, `float` |
| integer / int32 / int64 | `int`, `int32`, `int64` |
| array | list |
| object with typed `additionalProperties` | association list |
| empty object | validated empty-object type |
| `x-kubernetes-int-or-string` | integer-or-string variant |
| preserved or unsupported subtree | `Yojson.Safe.t` |

The checked-in CRD remains authoritative. The generator embeds its exact `spec`
and `status` schema JSON through `Kube_crd.Schema.raw`, so constraints such as
patterns, numeric bounds, defaults, descriptions, and Kubernetes extensions are
not weakened when OCaml types use a simpler representation. Multi-version CRDs
are copied intact while one selected version supplies the compiled OCaml type.

Representational unions without one structural base type, mixed `properties`
plus `additionalProperties`, embedded Kubernetes resources, untyped maps,
`$ref`, and preserved-unknown subtrees currently use the raw JSON fallback.
Future generator versions can make these shapes more precise without changing
their wire data.

## Input constraints

The CRD must:

- use `apiextensions.k8s.io/v1`;
- have one selected served version;
- require `.spec`;
- describe both `.spec` and `.status`; and
- enable the status subresource.

JSON input is fully supported. The self-contained YAML reader supports ordinary
block mappings/sequences, quoted and unquoted scalars, flow mappings/sequences,
comments, and literal/folded description blocks. It intentionally rejects
multiple documents, anchors, aliases, merge keys, and tags. For a manifest using
those features, provide its JSON form, for example from `kubectl get crd -o json`.

The repository test suite renders and validates a representative project. The
kind integration gate additionally runs the installed command, compiles its
output as a separate Dune project, installs its CRD and sample, verifies typed
decoding and finalizer reconciliation, deletes the object through cleanup, and
checks graceful shutdown.

In addition, `test/scaffold_acceptance.sh` generates and independently compiles
operators from pinned Gateway API, KEDA, and Prometheus Operator CRDs, then
initializes and compiles a type-first project with a clean CRD drift gate. The
fixtures, upstream release URLs, checksums, and licensing are recorded in
`scaffold/fixtures/real-world/README.md`. This corpus specifically guards YAML
parser compatibility and generator scalability against schemas produced by
multiple controller toolchains.
