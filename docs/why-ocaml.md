# Why write a Kubernetes operator in OCaml?

OCaml is compelling for an operator when the hard part is modeling policy and
state transitions correctly. It is not a claim that every Kubernetes team
should leave Go or Rust.

## The case for OCaml

### Desired state becomes an explicit data model

Variants make lifecycle states and outcomes closed and exhaustively matched.
Records make the data required by a transition visible. Options make absence
explicit instead of relying on nil values or partially initialized objects.
When a CRD evolves, the compiler points at the reconciliation branches and
tests that must change.

`[@@deriving kube]` takes this further: one OCaml model produces strict JSON
codecs and a structural CRD schema. The same module is the type checked by the
reconciler and the source of the deployed CRD manifest, with a drift check in
the build.

### The language is small and the control flow stays readable

Operator code is mostly parsing, decisions, retries, and state machines. OCaml's
pattern matching and expression-oriented error handling keep those paths close
to the domain language. The finalizer API is a concrete example: `Apply` and
`Cleanup` are the only cases, and forgetting one is a compile-time warning.

### Native deployment without a language runtime image

An operator builds to a native executable. The generated multi-stage container
runs that executable as a non-root user with a read-only root filesystem. OCaml
5 supplies multicore-capable system threads; ocaml-kube adds structured
cancellation so watches, workers, diagnostics, leadership, and shutdown share
one lifetime.

### Strong boundaries make tests useful

The public transport boundary lets tests run normal authentication, request
construction, JSON decoding, watches, caches, queues, and controller code while
replacing only the final exchange. This is a good fit for pure decision logic:
keep domain transitions as ordinary functions, test wire behavior with
`kube.test`, and reserve kind for Kubernetes semantics.

### It gives existing OCaml systems a direct control plane

Teams already using OCaml for compilers, static analysis, finance, formal
methods, or high-assurance services can share domain types and libraries with a
Kubernetes operator instead of maintaining a second-language control plane.
That organizational advantage is often more important than a microbenchmark.

## When Go is the better choice

Choose Go and client-go/controller-runtime when upstream immediacy, the largest
Kubernetes hiring pool, abundant examples, or a vendor's Go-only integration is
the dominant requirement. It is Kubernetes' native ecosystem and remains the
lowest-risk default for most teams.

## When Rust is the better choice

Choose Rust and kube-rs when memory control, zero-cost abstractions, Tokio
integration, or sharing a Rust systems codebase matters more than development
speed. Rust offers stronger low-level control and a substantially larger cloud
native ecosystem, with a correspondingly steeper ownership and async learning
curve.

## When OCaml is the right choice

OCaml is a particularly good fit when:

- the controller implements a non-trivial policy or state machine;
- correctness and reviewability matter more than ecosystem breadth;
- the team already has OCaml expertise or domain libraries;
- a compact native service and fast compile-test loop are valuable; and
- the remaining gaps in the [ecosystem comparison](ecosystem-comparison.md) do
  not affect the deployment.

The practical pitch is modest: Kubernetes gives every implementation the same
declarative API and reconciliation model. OCaml expresses that model unusually
well, and ocaml-kube now provides the operational shell needed to spend most of
the code on the operator's actual domain.
