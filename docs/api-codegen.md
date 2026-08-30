# Generated Kubernetes API

`ocaml-k8s` keeps protocol/runtime releases independent from schema releases.
Each Kubernetes minor is installed as its own library and OCaml module, so an
application opts into schema changes explicitly.

| Kubernetes source | Dune library | OCaml module | Stable resources | Definition closure |
| --- | --- | --- | ---: | ---: |
| v1.34.9 | `kube.api.v1_34` | `Kube_api_v1_34` | 58 | 461 |
| v1.35.6 | `kube.api.v1_35` | `Kube_api_v1_35` | 58 | 462 |
| v1.36.2 | `kube.api.v1_36` | `Kube_api_v1_36` | 60 | 473 |
| v1.37.0 | `kube.api.v1_37` | `Kube_api_v1_37` | 64 | 509 |

## Input and reproducibility

`codegen/kubernetes-versions.tsv` is the source of truth for supported schema
packages. Every row pins a Kubernetes minor, exact patch release, and SHA-256
of the official Swagger 2.0 document at:

```text
https://raw.githubusercontent.com/kubernetes/kubernetes/VERSION/api/openapi-spec/swagger.json
```

For each row, the repository checks in:

- `codegen/resources-VERSION.json`, the derived resource manifest;
- `codegen/openapi/VERSION.json`, the transitive schema closure;
- `api/MINOR/kube_api_MINOR.ml` and `.mli`, the generated API; and
- `api/MINOR/dune`, the package declaration and offline drift rule.

The generator derives resource definition, group, version, plural, and scope
from stable LIST and WATCH operations and their list response schemas. The
resource set is therefore a reviewable result, not a hand-maintained allowlist.
The closure reproduces generated output without a network connection. The full
multi-megabyte upstream documents are deliberately not committed.

## Generated representation

Every selected resource gets:

- a strongly typed record and all transitively referenced records;
- a constructor using required and optional labelled arguments;
- JSON decoding with field-local error context;
- deterministic JSON encoding that omits absent optional fields;
- a descriptor containing group, version, kind, plural, and scope; and
- a `Kube.Core.Resource` implementation for typed client and controller
  functors.

Each package exposes `all_resources`, a deterministic descriptor registry.
Matrix tests compile all supported packages and assert their size, GVR
uniqueness, scope, and representative core resources.

Common helpers are grouped under `Meta_v1`, `Int_or_string`, and `Quantity`.
Kubernetes `int64` values use OCaml `int64`; `IntOrString` is represented as
`` `Int of int32 | `String of string ``; quantities and timestamps remain opaque
wire-format strings. Map fields use `(string * 'a) list`, preserving stable
encoding order. Objects that intentionally preserve arbitrary content, such as
`FieldsV1`, remain `Yojson.Safe.t` and round-trip unknown fields.

Typed records intentionally discard unknown object fields. Code that proxies a
newer resource without losing fields should use `Kube.Dynamic`.

## Using a package

```ocaml
module K8s = Kube_api_v1_37
module Deployments = Kube.Client.For (K8s.Apps_v1.Deployment)
module Deployment_controller = Kube.Controller.Make (K8s.Apps_v1.Deployment)

let metadata =
  K8s.Meta_v1.ObjectMeta.make ~name:"operator" ~namespace:"default" ()

let deployment =
  K8s.Apps_v1.Deployment.make ~api_version:"apps/v1" ~kind:"Deployment"
    ~metadata ()
```

Nested spec constructor names include their complete upstream definition path.
That is verbose at the lowest layer but prevents collisions between identical
type names from different API groups and versions.

## Regeneration and maintenance

Regenerate every pinned line:

```sh
codegen/update-kubernetes-api.sh all
```

Regenerate one minor after changing its row:

```sh
codegen/update-kubernetes-api.sh 1.37
```

The script downloads immutable upstream documents, verifies every checksum,
bootstraps manifests directly from source metadata, creates package directories
and Dune rules, regenerates closure and OCaml files, and finishes with the full
offline drift check. It has no JSON-command-line dependency.

To verify upstream inputs and generated outputs without changing files:

```sh
codegen/update-kubernetes-api.sh --check all
```

For a network-free verification of checked-in inputs and generated files:

```sh
opam exec -- dune build @codegen-check
```

The small golden fixture under `codegen/fixtures/` fixes generator behavior.
Runtime tests cover constructors, required-field failures, `IntOrString`, raw
JSON preservation, and all versioned registries.

## Versioning policy

A Kubernetes schema minor always receives a new sublibrary rather than
rewriting another minor's types. Applications can update the runtime and schema
packages independently.

Patch updates may regenerate a minor package before its first stable
`ocaml-k8s` release. Once published, a source-incompatible schema change must
use a new package line or a new major project release; an existing public module
must not change silently.
