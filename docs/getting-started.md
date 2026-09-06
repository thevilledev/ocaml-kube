# Getting started

ocaml-kube requires OCaml 5.1 or newer. Install the released package with:

```sh
opam install kube
```

To build the development checkout instead, install its dependencies and run
the complete local build:

```sh
opam install . --deps-only --with-test --with-doc
opam exec -- dune build @all @install @doc @codegen-check
opam exec -- dune runtest
```

## Configure a client

`Kube.Config.load_default` uses in-cluster service-account credentials when they
are available. Otherwise it loads `KUBECONFIG` or the standard user
configuration:

```ocaml
let config = Kube.Config.load_default () |> Result.get_ok
let client = Kube.Client.create config
```

The kubeconfig loader supports token, token-file, basic, client-certificate,
and exec-plugin credentials. It also handles context selection, merged
kubeconfigs, HTTP and SOCKS5 proxies, and user impersonation.

Programmatic configuration is available when kubeconfig is not appropriate:

```ocaml
let config =
  Kube.Config.make
    ~credential:(Kube.Config.Static_token token)
    (Uri.of_string "https://kubernetes.example.com")

let client =
  Kube.Client.create ~connect_timeout:10. ~write_timeout:30.
    ~response_header_timeout:30. config
```

Long-running bodies such as watches and logs do not have a wall-clock deadline.
Pass a cancellation token to bound their lifetime.

## Make a typed request

Select the generated API package that matches the Kubernetes schema against
which the application is compiled:

```ocaml
module K8s = Kube_api_v1_36
module Config_maps = Kube.Client.For (K8s.Core_v1.ConfigMap)

let config_map =
  Config_maps.get client ~namespace:"default" "application-settings"
  |> Result.get_ok
```

See [Client APIs](client.md) for generated types, discovery, dynamic resources,
subresources, logs, and upgraded connections.

## Create an operator

Generate an OCaml-first project:

```sh
ocaml-kube init \
  --output widget-operator \
  --group example.dev \
  --kind Widget
```

Or import an existing structural CRD:

```sh
ocaml-kube scaffold \
  --crd deploy/widget-crd.yaml \
  --output widget-operator
```

The generated project contains typed models, a controller, CRD and RBAC
manifests, deployment files, drift checks, standard process lifecycle, and a
bounded custom-metrics example. Its entry point only needs to construct the
application-specific controller; configuration, graceful signals, diagnostics,
and optional Lease leadership are handled by `Kube.Operator`. See
[Operator scaffolding](scaffolding.md) for the supported schema mapping and
generated layout, then [Operator patterns](operator-patterns.md) for owned
children, status conditions, conflict retries, and testing.

## Run the integration example

Create a local cluster and run the proof operator:

```sh
kind create cluster \
  --name ocaml-kube-poc \
  --kubeconfig "$PWD/kubeconfig.kind"
kubectl --kubeconfig "$PWD/kubeconfig.kind" apply -f deploy/crd.yaml
opam exec -- dune exec examples/greeting_operator.exe -- \
  --kubeconfig "$PWD/kubeconfig.kind"
```

The assertion-driven integration suite performs the same setup and verifies
reconciliation, status updates, watches, and finalization:

```sh
test/integration_kind.sh
```
