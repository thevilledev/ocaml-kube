# Real-world scaffold acceptance corpus

These CRDs are unmodified, pinned upstream release artifacts used to ensure
that `ocaml-kube scaffold` accepts controller-generated YAML, preserves the
source CRD, produces deterministic types, and emits an independently buildable
operator project. They are test inputs, not endorsed deployment manifests.

| Fixture | Upstream release source | SHA-256 |
| --- | --- | --- |
| `gateway-api-gatewayclass-v1.5.1.yaml` | [Gateway API v1.5.1 GatewayClass](https://raw.githubusercontent.com/kubernetes-sigs/gateway-api/v1.5.1/config/crd/standard/gateway.networking.k8s.io_gatewayclasses.yaml) | `234ad4b2757ee1b3596a4187bb98df071c5bbdd9e1806f692f2dbf8387e6bef4` |
| `keda-scaledobject-v2.20.1.yaml` | [KEDA v2.20.1 ScaledObject](https://raw.githubusercontent.com/kedacore/keda/v2.20.1/config/crd/bases/keda.sh_scaledobjects.yaml) | `af35555862a83933b074af14ad439c4b4ceabaf4daa3e9338e9b23e36d308d80` |
| `prometheus-servicemonitor-v0.93.0.yaml` | [Prometheus Operator v0.93.0 ServiceMonitor](https://raw.githubusercontent.com/prometheus-operator/prometheus-operator/v0.93.0/example/prometheus-operator-crd/monitoring.coreos.com_servicemonitors.yaml) | `a99047972c9dd7679ce2050b9968e8a08773492e671b3e17abd476d42aa78a32` |

The three upstream projects distribute these files under the Apache License
2.0. See [`LICENSE-APACHE-2.0`](LICENSE-APACHE-2.0). When refreshing a fixture,
update the release in its filename, source URL, checksum, expectations in
`scaffold/test_scaffold.ml`, and the external-build cases in
`test/scaffold_acceptance.sh` together.
