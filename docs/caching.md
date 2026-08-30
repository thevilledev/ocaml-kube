# Cache scope and recovery

Every typed controller reads through a reflector-backed store. Scope is fixed
when the cache is created so absence has a precise meaning for cached reads.

| Configuration | API streams | Store |
| --- | --- | --- |
| `Cache.create ()` | One all-namespace stream | Every namespace returned by the server |
| `Cache.create ~namespace:"team-a" ()` | One `team-a` stream | Only `team-a` |
| `Cache.create ~namespaces:[ "team-a"; "team-b" ] ()` | One stream per selected namespace | The union of both snapshots |

Cluster-scoped resources accept only the default form. Empty, blank, or
duplicate namespace selections are rejected, and `namespace` cannot be combined
with `namespaces`.

## Multi-namespace invariants

Each selected namespace owns its LIST/WATCH loop, resource version, reconnect
backoff, and compaction recovery. The loops share one store and serialized event
dispatch. This provides the following guarantees:

- readiness is published only after every selected namespace has installed its
  first complete paginated LIST;
- a 410 response relists and atomically replaces only the affected namespace;
- objects and secondary-index memberships from other namespaces survive that
  replacement;
- relist transitions retain previous values, so ownership or index relationship
  changes enqueue both the old and new primary keys;
- every LIST item and watch event is checked against the namespace of its API
  route; a mismatch is a terminal decode error;
- a terminal failure cancels and joins sibling loops and makes the whole cache
  unready; and
- external cancellation joins every namespace loop before the component stops.

One all-namespace stream remains the simplest and least expensive choice when
the service account can list the entire resource collection. Explicit
multi-namespace streams are useful when RBAC grants only selected namespaces or
when independent recovery is operationally preferable. They cost one watch
connection and initial LIST per namespace.

## Controllers and watched resources

The same scope forms are supported by primary, secondary, and owner-reference
watches:

```ocaml
module Controller = Kube.Controller.Make (Greeting)
module Owns_deployment = Controller.Owns (Kube_api_v1_36.Apps_v1.Deployment)

let namespaces = [ "team-a"; "team-b" ]
let cache = Controller.Cache.create ~namespaces ()
let deployments = Owns_deployment.make ~namespaces ()

let component =
  Controller.component ~cache ~watches:[ deployments ] ~reconcile ()
```

Passing scope or selector arguments together with an already-created cache is
rejected. This prevents the controller declaration from implying a different
read boundary from the cache it actually receives. Share the same cache value
between components to deduplicate its LIST/WATCH dependency.

Pass the diagnostics health registry as `~health` to publish a
`<controller-name>/cache-sync` readiness check. The check fails from component
construction through the initial LISTs and watch establishment. It succeeds
only after every primary and watched cache is synchronized, initial keys are
seeded, and reconciliation workers have started. Cancellation or component
shutdown immediately makes it fail again.

`request.reader` reads exactly the synchronized primary cache. A missing key is
reported as `NotFound`; it never falls through to a live request because a
filtered cache cannot establish that the object is absent from the API server.
Use `fresh_get` or `fresh_list_all` when the reconciliation decision explicitly
requires current server state.

## Verification

`test/test_multi_namespace.ml` covers atomic namespace replacement, secondary
indexes, concurrent reads over 50,000 objects, isolated 410 recovery, malformed
LIST and watch namespaces, controller sources, and owner mappings. The kind
integration scenario creates two namespaces and proves initial reconciliation,
an update in one namespace, a deletion in the other, and graceful shutdown
against a real Kubernetes API server.
