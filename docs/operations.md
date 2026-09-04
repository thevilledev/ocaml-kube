# Operations

Operational components share the same structured cancellation scope as
controllers. A serving, bind, or leadership failure can therefore stop the
manager cleanly instead of leaving partial background work running.

## Logging

Library logging is disabled by default. A logger supplied to the client is
inherited by reflectors, controllers, managers, leader election, diagnostics,
and webhook servers:

```ocaml
let logger = Kube.Log.stderr ~min_level:Kube.Log.Info ()
let client = Kube.Client.create ~logger config
```

`Log.stderr` writes one JSON object per line. `Log.with_name` and
`Log.with_fields` add immutable context, and `Log.set_min_level` updates the
threshold shared by derived loggers. Custom sinks are serialized and isolated
from controller work if they fail.

Transport logs exclude query strings, headers, credentials, bodies, and API
error messages. Application fields that must remain opaque can use
`Log.Redacted`.

## Health and metrics

Diagnostics expose `/healthz`, `/readyz`, and `/metrics` through a supervised
native HTTP server:

```ocaml
let health = Kube.Health.create ()
let metrics = Kube.Metrics.create ()

let diagnostics =
  Kube.Diagnostics.create ~address:"0.0.0.0" ~port:8080
    ~health ~metrics ()

Kube.Manager.add manager (Kube.Diagnostics.component diagnostics)
```

Controller components can publish reconciliation counts, latency histograms,
and active-worker gauges to the same registry. Passing the health registry to a
controller registers a cache-sync readiness check. It becomes ready only after
caches synchronize, initial keys are queued, and workers start.

Leader-elected replicas remain live while standby readiness is false. Readiness
also becomes false as soon as shutdown begins.

## Application instrumentation

Controllers automatically publish reconciliation counts, failures, latency,
and active workers when given a registry. Add domain outcomes to that same
registry with bounded labels:

```ocaml
let outcome registry value =
  Kube.Metrics.Counter.create ~registry
    ~name:"greeting_reconciles_total"
    ~help:"Greeting reconciliation outcomes."
    ~labels:[ ("outcome", value) ] ()

let updated = outcome metrics "updated"
let unchanged = outcome metrics "unchanged"
```

Do not use object names, UIDs, messages, or arbitrary labels as Prometheus label
values. Put high-cardinality object context in structured logs and Events. The
[`greeting_operator`](../examples/greeting_operator.ml) is a complete example
with outcome counters, a message-size histogram, contextual logs, Events, and
the diagnostics endpoint that exposes them.

## Kubernetes Events

The event recorder does not depend on a generated built-in API package:

```ocaml
let recorder =
  Kube.Events.create ~client
    ~reporting_controller:"demo.example.com/foo-controller"
    ~reporting_instance:pod_uid ()
  |> Result.get_ok

let regarding = Kube.Core.object_reference Foo.api (Foo.metadata foo) in
Kube.Events.record recorder ~regarding ~type_:Kube.Events.Normal
  ~reason:"Reconciled" ~action:"UpdateStatus" ~note:"Foo is ready"
```

Event publication is best effort. Report delivery failures through logging or
metrics without treating them as failure of the reconciled operation.

## Webhooks

Admission and conversion handlers run on the bounded TLS webhook server and can
join the same manager, health, and metrics components. See
[Admission and conversion webhooks](webhooks.md) for TLS, optional client
certificates, readiness, mutation patches, and conversion identity checks.
