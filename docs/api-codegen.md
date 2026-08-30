# Generated Kubernetes API

`ocaml-k8s` keeps Kubernetes protocol/runtime releases independent from schema
releases. The first generated schema library is installed as
`kube.api.v1_36` and exposed through the OCaml module `Kube_api_v1_36`.

## Input and reproducibility

The package is generated from Kubernetes v1.36.2's official Swagger 2.0
document:

- source: <https://raw.githubusercontent.com/kubernetes/kubernetes/v1.36.2/api/openapi-spec/swagger.json>
- complete upstream SHA-256:
  `dcede2063da1d7ad62ecb5af8adb6d7fabd0b52385a7fa0048afb491dac90450`
- derived resource manifest: `codegen/resources-v1.36.2.json`
- checked-in transitive schema closure: `codegen/openapi/v1.36.2.json`

The manifest contains all 60 stable resources whose upstream paths expose both
LIST and WATCH operations. Their references produce a closure of 473
definitions. The generator derives each resource definition, group, version,
plural, and namespace scope from the operations and the LIST response schema;
the manifest is a checked-in, reviewable result rather than a hand-maintained
allowlist. The closure is sufficient to reproduce generated output without
network access, so ordinary builds do not download Kubernetes sources or
execute the generator.

The full upstream document is deliberately not committed. The closure is much
smaller, and its provenance remains bound to the immutable URL and checksum in
the manifest and generated headers.

## Generated representation

Every selected resource gets:

- a strongly typed record and all transitively referenced records;
- a constructor using required and optional labelled arguments;
- JSON decoding with field-local error context;
- deterministic JSON encoding that omits absent optional fields;
- a resource descriptor containing its group, version, kind, plural, and scope;
- an implementation of `Kube.Core.Resource` suitable for the typed client and
  controller functors.

The package also exposes `all_resources`, a deterministic registry of the 60
generated descriptors. Tests assert its size, GVR uniqueness, representative
scope decisions, and the presence of newer API groups.

Common helpers are grouped under `Meta_v1`, `Int_or_string`, and `Quantity`.
Kubernetes `int64` values use OCaml `int64`; `IntOrString` is represented as
`` `Int of int32 | `String of string ``; quantities and timestamps remain opaque
wire-format strings. Map fields use `(string * 'a) list`, preserving a stable
encoding order. Schema objects that intentionally preserve arbitrary content,
such as `FieldsV1`, remain `Yojson.Safe.t` and round-trip unknown fields.

Typed records intentionally discard unknown object fields. Applications that
must proxy a newer resource without losing fields should use `Kube.Dynamic`,
which retains the complete original JSON value.

## Using the package

```ocaml
module K8s = Kube_api_v1_36
module Deployments = Kube.Client.For (K8s.Apps_v1.Deployment)
module Deployment_controller = Kube.Controller.Make (K8s.Apps_v1.Deployment)

let metadata =
  K8s.Meta_v1.ObjectMeta.make ~name:"operator" ~namespace:"default" ()

let deployment =
  K8s.Apps_v1.Deployment.make ~api_version:"apps/v1" ~kind:"Deployment"
    ~metadata ()
```

Nested spec constructors are also exposed. Their generated names are globally
unique because they include the complete upstream definition path. This is
verbose at the lowest layer but prevents collisions between identically named
types from different API groups and versions.

## Regeneration

Run:

```sh
codegen/update-kubernetes-api.sh
```

The script downloads the immutable upstream document, verifies its checksum,
derives the complete stable LIST/WATCH resource manifest, regenerates the schema
closure and both OCaml files, and runs the drift check. It refuses to generate
when the downloaded bytes do not match the pinned checksum.

For an offline verification of checked-in files, run:

```sh
opam exec -- dune build @codegen-check
```

Generator behavior is additionally fixed by the small golden fixture under
`codegen/fixtures/`. Runtime tests construct and round-trip generated resources,
exercise required-field failures, verify `IntOrString`, preserve raw JSON
objects, and validate all 60 resource descriptors. The resource-derivation
fixture additionally proves stable-version filtering, scope inference,
deterministic ordering, and manifest metadata preservation.

## Versioning policy

A Kubernetes schema minor receives a new sublibrary rather than rewriting the
types of an existing one. For example, a future schema line would use a distinct
public library and OCaml module. Applications can therefore choose when to move
between generated schemas while continuing to update the protocol/runtime
library independently.

Patch updates within one Kubernetes minor may regenerate the same sublibrary
before its first stable release. After a stable `ocaml-k8s` release, any
source-incompatible schema change requires a new generated sublibrary or a new
major project release; it must not silently change an already published API.
