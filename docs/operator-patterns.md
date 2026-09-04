# Operator patterns

This guide is the shortest path from a generated custom resource to an
idempotent, observable operator. Every API shown here is exercised by the test
suite; the complete snippets live in
[`examples/common_patterns.ml`](../examples/common_patterns.ml) and
[`examples/greeting_operator.ml`](../examples/greeting_operator.ml).

## Start with the operator shell

`Kube.Operator` owns the repetitive process lifecycle: kubeconfig or in-cluster
configuration, signal cancellation, client cleanup, diagnostics, Lease leader
election, health, and the metrics registry.

```ocaml
let options =
  Kube.Operator.Options.parse ~name:"widget-operator"
    ~leader_election_name:"widget-operator" ()

let result =
  Kube.Operator.run options ~components:(fun context ->
      [
        Widget_controller.component ?namespace:context.namespace
          ~workers:context.workers ~health:context.health
          ~metrics:context.metrics ~reconcile ();
      ])
```

The same binary runs locally with leader election and diagnostics disabled, or
in a Deployment with:

```sh
widget-operator \
  --leader-elect \
  --leader-election-name widget-operator \
  --diagnostics-address 0.0.0.0 \
  --diagnostics-port 8080
```

Use `Operator.run_with_client` in an embedding application or deterministic
test that already owns its client. It performs no signal installation and does
not close the supplied client.

## Apply an owned child

Server-Side Apply is the most direct way to state the fields an operator owns.
`Kube.Reconcile.Make` infers the desired object's name and namespace, applies a
stable field manager, and can add a validated controller owner reference before
the write:

```ocaml
module K8s = Kube_api_v1_36
module Config_maps = Kube.Reconcile.Make (K8s.Core_v1.ConfigMap)

let apply_message client cancel (greeting : Greeting.t) =
  let namespace = Option.value ~default:"default" greeting.metadata.namespace in
  let metadata =
    K8s.Meta_v1.ObjectMeta.make ~name:(greeting.metadata.name ^ "-message")
      ~namespace ()
  in
  let desired =
    K8s.Core_v1.ConfigMap.make ~api_version:"v1" ~kind:"ConfigMap"
      ~metadata ~data:[ ("message", greeting.spec.message) ] ()
  in
  Config_maps.apply_owned ~cancel client ~field_manager:"greeting-operator"
    ~owner_api:Greeting.api ~owner:greeting.metadata desired
```

An existing controller owner is never silently replaced. Cross-namespace
ownership and a namespaced owner for a cluster-scoped child are rejected before
network I/O. Set `~force:true` only when the controller intentionally takes
field ownership from another manager.

Connect child changes back to the owner with `Owns`:

```ocaml
module Controller = Kube.Controller.Make (Greeting)
module Owns_config_maps = Controller.Owns (K8s.Core_v1.ConfigMap)

let component ~metrics ~health ~reconcile =
  Controller.component ~watches:[ Owns_config_maps.make () ]
    ~metrics ~health ~reconcile ()
```

## Retry an optimistic update

When an update depends on a fresh server value, refetch and recompute it on
every conflict. `Kube.Retry.on_conflict` deliberately returns non-conflict
errors immediately and respects cancellation:

```ocaml
Kube.Retry.on_conflict ~cancel:request.cancel (fun () ->
    match Widget_api.get ~cancel:request.cancel client
            ?namespace:request.key.namespace request.key.name with
    | Error _ as error -> error
    | Ok current ->
        let updated = recompute current in
        Widget_api.replace ~cancel:request.cancel client
          ?namespace:request.key.namespace request.key.name updated)
```

Do not retry a stale precomputed object: the new GET is what makes a conflict
retry safe. For ordinary controller failures, return `Error` and let the keyed
controller queue apply its longer-lived error policy.

## Report standard status conditions

`Kube_crd.Condition` implements the Kubernetes `metav1.Condition` wire shape
and transition semantics. It can be embedded directly in a derived status:

```ocaml
module Status = struct
  type t = {
    observed_generation : int64;
    conditions : Kube_crd.Condition.t list;
  }
  [@@deriving kube]
end
```

Set by condition type. A reason or message change preserves
`lastTransitionTime`; a status change advances it:

```ocaml
let ready =
  Kube_crd.Condition.make ~type_:"Ready" ~status:Kube_crd.Condition.True
    ~observed_generation:generation ~reason:"Available"
    ~message:"All managed resources are ready" ()

let conditions, changed = Kube_crd.Condition.set ready old_conditions
```

Patch the status subresource only when the desired status differs. Recording
`observedGeneration` tells users exactly which desired generation the status
describes.

## Finalize external state

Make the finalizer helper the outer operation in the reconciler:

```ocaml
Finalizer.run ~cancel:request.cancel client resource finalizer (function
  | Finalizer.Apply current -> converge current
  | Finalizer.Cleanup deleting -> cleanup_external_state deleting)
```

The helper persists the finalizer before calling `Apply`, calls `Cleanup` only
for deleting resources, and removes the finalizer only after cleanup returns
`Done`. Cleanup must be idempotent because any controller can restart between
steps.

## Instrument behavior, not object identity

Controller components automatically publish reconcile totals, failures,
duration, and active workers. Application metrics should describe domain
outcomes with a small, bounded label set:

```ocaml
let outcome registry value =
  Kube.Metrics.Counter.create ~registry
    ~name:"widget_reconciles_total"
    ~help:"Widget reconciliation outcomes."
    ~labels:[ ("outcome", value) ] ()

let updated = outcome context.metrics "updated"
let unchanged = outcome context.metrics "unchanged"
```

Avoid labels containing resource names, UIDs, messages, or arbitrary user
input; those create unbounded Prometheus cardinality. Use structured logs and
Kubernetes Events for per-object details. The Greeting example combines all
three: bounded custom metrics, contextual JSON logs, and best-effort Events.

## Test the reconciliation boundary

Inject `Kube_test.Transport` and run the production request construction and
decoding path:

```ocaml
let transport =
  Kube_test.Transport.scripted
    [ Kube_test.Transport.respond_json applied_object ]

let client = Kube_test.client transport
let result = Config_maps.apply_owned client ~field_manager:"widget-operator"
    ~owner_api:Widget.api ~owner:widget.metadata desired

let request = List.hd (Kube_test.Transport.requests transport)
```

Assert the target, field manager, request body, and resulting status. Keep a
small kind-based integration test for API-server behavior such as defaulting,
admission, field ownership, and garbage collection.
