# Controller runtime

The controller runtime combines typed resources, LIST/WATCH reflectors, indexed
caches, dirty-key work queues, retry policies, and structured cancellation.

## Run an operator

`Kube.Operator` supplies the standard process shell without hiding controller
components. It loads local or in-cluster configuration, installs graceful
signal cancellation, owns the client, optionally serves diagnostics, and runs
controllers under optional Lease leadership:

```ocaml
let options =
  Kube.Operator.Options.parse ~name:"greeting-operator"
    ~leader_election_name:"greeting-operator" ()

Kube.Operator.run options ~components:(fun context ->
    [
      Greeting_controller.component ?namespace:context.namespace
        ~workers:context.workers ~health:context.health
        ~metrics:context.metrics ~reconcile ();
    ])
```

The callback receives one client, cancellation token, health registry, metrics
registry, process identity, namespace, and worker count. Components remain the
ordinary composable `Manager.component` values described below.

## Reconcile a resource

A resource module defines API identity, metadata access, and JSON codecs. The
controller functor turns it into typed reconciliation requests:

```ocaml
module Controller = Kube.Controller.Make (Greeting)

let reconcile client (request : Controller.request) =
  match request.resource with
  | None -> Ok Kube.Controller.Done
  | Some greeting ->
      let current =
        Controller.Cached.get_by_key request.reader request.key
      in
      (* Pass request.cancel to blocking client operations. *)
      ignore (client, greeting, current);
      Ok (Kube.Controller.Requeue_after 30.)
```

Requests carry the latest cached resource, its stable namespace/name key, a
cache-backed reader, and the controller cancellation token.

## Run components

The manager owns one cancellation scope and deduplicates shared reflector
dependencies:

```ocaml
let cache = Controller.Cache.create ()

let component =
  Controller.component ~cache ~name:"greeting-status" ~workers:4
    ~reconcile ()

let run cancel client =
  let manager = Kube.Manager.create ~cancel client in
  Kube.Manager.add manager component;
  Kube.Manager.run manager
```

Keys make reconciliation level-triggered. An update that arrives during a run
marks the key dirty and schedules one later pass without allowing concurrent
processing of the same object.

## Scope and cache reads

Controllers watch all namespaces by default. Use `~namespace` for one namespace
or `~namespaces` for an explicit set. Each selected namespace maintains its own
resource version and recovery loop while sharing one typed cache:

```ocaml
let cache =
  Controller.Cache.create ~namespaces:[ "team-a"; "team-b" ] ()
```

The cache becomes ready only after every initial snapshot is installed.
`Controller.Cached.get`, `list`, and `by_index` never issue HTTP requests. Use
`fresh_get` or `fresh_list_all` when an API-server read is intentional.

`Controller.Watches(Resource)` maps secondary events to primary keys.
`Controller.Owns(Resource)` uses Kubernetes controller owner references. All
primary and secondary caches synchronize before workers start. See
[Cache scope and recovery](caching.md) for recovery invariants and shared cache
behavior.

## Deadlines and shutdown

Controller startup bounds cache synchronization to 120 seconds by default.
`~cache_sync_timeout` changes that limit, while `~reconcile_timeout` gives each
reconciliation a linked child token. Cancellation remains cooperative: every
blocking operation must receive `request.cancel`, and CPU-bound work must check
it.

## Leader election

High-availability replicas can run the manager under Kubernetes Lease
leadership:

```ocaml
let election =
  Kube.Leader_election.default ~namespace:"default"
    ~name:"greeting-operator" ~identity:pod_identity

Kube.Leader_election.run ~cancel client election (fun leadership_cancel ->
    let manager = Kube.Manager.create ~cancel:leadership_cancel client in
    Kube.Manager.add manager component;
    Kube.Manager.run manager)
```

Every replica needs a unique identity. The manager is joined before a graceful
Lease release. Lease leadership follows the Kubernetes single-active-controller
convention; it is not fencing for external systems.

## Finalizers and events

`Controller.Finalizer(Resource).run` persists a finalizer before normal work,
runs cleanup for deleting objects, and removes the finalizer only after cleanup
completes.

Controllers can publish supplemental `events.k8s.io/v1` Events through
`Kube.Events`. Event delivery is best-effort diagnostics and should not change
the outcome of the operation being reported. See [Operations](operations.md)
for logging, health, metrics, and diagnostics integration.

For complete reconciliation patterns—including owned-child Server-Side Apply,
conflict retries, standard status conditions, and tests—see
[Operator patterns](operator-patterns.md).
