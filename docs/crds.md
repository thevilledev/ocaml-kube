# Custom resources and CRD generation

The `kube.crd` and `kube.ppx` sublibraries keep a custom resource's runtime
representation and Kubernetes structural schema in one OCaml module. Manifest
generation has no network dependency: manifests are deterministic values
produced from the checked-in source definition.

## Schema API

`Kube_crd.Schema` represents the structural OpenAPI subset used by
`apiextensions.k8s.io/v1` CRDs. It supports strings and constraints, integer and
number formats, booleans, arrays, objects, typed maps, alternatives,
Int-or-String, defaults, nullability, descriptions, and preservation of unknown
JSON objects. `Schema.raw` is an explicit escape hatch for Kubernetes extensions
that do not yet have a typed constructor.

Validation rejects duplicate properties and enum values, missing required-field
schemas, malformed alternatives, non-object raw schemas, and the non-structural
combination of named properties with typed `additionalProperties`.

## CRD manifests

`Kube_crd.Custom_resource_definition` builds a v1 CRD with one or more served
versions. It represents status and scale subresources, short names, categories,
and additional printer columns. Construction validates DNS names, Kind and
version syntax, unique versions, exactly one served storage version, structural
root schemas, JSONPath fields, and printer-column constraints.

Both JSON and deterministic YAML encoders are available. Generated YAML is
ordinary Kubernetes input, not a bespoke template format.

## Typed resource functor

`Kube_crd.Resource.Make` accepts typed `Spec` and `Status` modules containing a
schema and JSON codec. Its result implements `Kube.Core.Resource`, so it can be
passed directly to `Kube.Client.For`, `Kube.Controller.Make`, reflectors, stores,
and finalizers. It also provides:

- a constructor with the correct API version and Kind defaults;
- strict spec and status decoding;
- `with_status` and `status_merge_patch` helpers; and
- the validated CRD manifest for the resource.

The Greeting proof in `examples/greeting.ml` is the executable reference. The
`examples/greeting_crd.exe` generator produces `deploy/crd.yaml`, and the
`@codegen-check` alias verifies that the committed manifest has not drifted.

## Standard conditions

`Kube_crd.Condition.t` implements the Kubernetes `metav1.Condition` wire shape
and provides a structural schema, strict codec, constructors, lookup, removal,
and `set` transition semantics. It can be used directly inside a type derived
with `[@@deriving kube]`. Updating a reason or message retains the previous
transition timestamp while changing status advances it. See
[Operator patterns](operator-patterns.md) for a status example.

## Deriving codecs and schemas

Add the rewriter to the module containing the custom-resource model:

```lisp
(preprocess
 (pps kube.ppx))
```

A type named `t` receives `to_json`, `of_json`, and `schema`; other names receive
`<name>_to_json`, `<name>_of_json`, and `<name>_schema`. The deriver also emits
those declarations from an `.mli`.

`[@@deriving kube_json]` emits only the JSON encoder and decoder. It is useful
when an exact schema comes from another source, as in CRD-imported scaffold
projects, and avoids constructing an unused recursively expanded schema.

```ocaml
module Spec = struct
  type t = {
    replicas : int;
    image : string
      [@kube.schema Kube_crd.Schema.string ~min_length:1 ()];
    pull_policy : string option;
  }
  [@@deriving kube]
end
```

The stable representation rules are:

- records are JSON objects and snake-case labels become lower-camel keys;
- `[@kube.key "jsonName"]` overrides a field name;
- optional record fields are omitted for `None` and are not required by the
  schema;
- lists and arrays are JSON arrays;
- `(string * 'a) list` is a typed JSON object map;
- tuples are structural objects with `item0`, `item1`, and subsequent fields;
- a variant containing only nullary constructors is a string enum;
- a variant with any payload is an object with a `type` enum and optional
  `value` object; unary payloads use `value.value`, tuple payloads use
  `value.item0` and later fields, and inline-record payloads retain their field
  names; and
- `[@kube.name "wire-name"]` overrides a constructor tag.

`[@kube.description "..."]` decorates a generated field schema.
`[@kube.schema expression]` replaces a field's inferred schema while leaving
its codec derived. This supports Kubernetes-specific extensions and is required
at the boundary of a recursive or mutually recursive type: codecs can recurse,
but Kubernetes structural schemas cannot contain recursive references.

The deriver rejects duplicate wire names, type parameters, polymorphic variants,
functions, objects, first-class modules, and unbounded schema recursion with a
source-located error. Heterogeneous variant payloads deliberately fall back to a
preserved object schema while retaining strict generated decoding. More precise
union validation and generated CEL rules remain future work.

Resource identity still needs explicit group, version, Kind, plural, and scope
metadata because those values cannot be inferred safely from an OCaml type.
