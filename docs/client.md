# Client APIs

The client supports generated resource modules, user-defined resources, and
resources discovered at runtime. Every path uses the same authentication,
transport, rate limiting, cancellation, and error classification.

## Generated resources

The `kube.api.v1_34` through `kube.api.v1_37` packages contain records,
constructors, JSON codecs, and resource descriptors generated from pinned
official OpenAPI specifications:

```ocaml
module K8s = Kube_api_v1_36

let metadata =
  K8s.Meta_v1.ObjectMeta.make ~name:"settings" ~namespace:"default" ()

let config_map =
  K8s.Core_v1.ConfigMap.make ~api_version:"v1" ~kind:"ConfigMap"
    ~metadata ~data:[ ("mode", "production") ] ()

module Config_maps = Kube.Client.For (K8s.Core_v1.ConfigMap)
```

Generated packages are independently selectable. Updating the client library
does not force an application onto a newer Kubernetes schema. See
[Generated Kubernetes API](api-codegen.md) for versioning and regeneration.

## Typed operations

`Kube.Client.For` provides typed GET, LIST, CREATE, UPDATE, DELETE, patch,
status, Scale, log, watch, and generic subresource operations. Paginated lists,
watch bookmarks, streaming initial events, and resource-version recovery are
handled explicitly.

Collection deletion defaults to the configured namespace, or `default`.
Cross-namespace collection deletion requires `~all_namespaces:true` and cannot
be combined with `~namespace`.

## Discovery and dynamic resources

Discovery resolves a group, version, kind, or resource into a `Kube.Core.api`
descriptor:

```ocaml
let mapper = Kube.Discovery.Mapper.create client

let deployment =
  Kube.Discovery.Mapper.resolve_gvk mapper
    ~group:"apps" ~version:"v1" ~kind:"Deployment"

let get name =
  match deployment with
  | Error error -> Error error
  | Ok mapping ->
      Kube.Dynamic.get client ~api:mapping.api ~namespace:"default" name
```

Exact lookups cache only the requested group-version. Preferred lookups honor
server-advertised versions and reject ambiguous unqualified names rather than
guessing. Cache lifetime is controlled with `invalidate` and `refresh`.

## Scale and logs

Scale uses the common `autoscaling/v1` representation:

```ocaml
module Deployments = Kube.Client.For (K8s.Apps_v1.Deployment)

let set_replicas client name replicas =
  match Deployments.get_scale client ~namespace:"default" name with
  | Error _ as error -> error
  | Ok scale ->
      let scale =
        { scale with spec = { Kube.Client.replicas = Int32.of_int replicas } }
      in
      Deployments.replace_scale client ~namespace:"default" name scale
```

Pod logs can be read with an explicit response-size bound or consumed as a
stream. Continuing streams stop through cancellation:

```ocaml
module Pods = Kube.Client.For (K8s.Core_v1.Pod)

let follow cancel client name =
  let options =
    { Kube.Client.default_log_options with
      container = Some "operator";
      follow = true }
  in
  Pods.stream_logs ~cancel ~options client ~namespace:"default" name
    ~on_chunk:(output_string stdout)
```

## Upgraded connections

`Kube.Remote_command` implements Kubernetes exec and attach, including terminal
resize, stdin half-close, and structured exit status. `Kube.Port_forward`
multiplexes Pod error and data streams and can expose them through supervised
local TCP listeners.

See [Streaming subresources](streaming.md) for API examples and operational
limits.
