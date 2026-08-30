# Kubernetes compatibility

## Policy

The client targets every Kubernetes minor release that upstream still lists as
an active branch. The normal upstream window is the newest three minors; a brief
four-release overlap can occur around a new minor release.

As of 2026-08-30, the active server minors are 1.34, 1.35, 1.36, and 1.37.
Kubernetes 1.34 reaches end of life on 2026-10-27. The dates and active branches
come from the upstream [patch-release table](https://kubernetes.io/releases/patch-releases/)
and [version-skew policy](https://kubernetes.io/releases/version-skew-policy/).

Compatibility means that the same source release passes the complete custom
resource integration scenario against the minor's real API server. It does not
mean that every alpha feature or every historical built-in API version is
available.

| Kubernetes | Status | Test environment |
| --- | --- | --- |
| 1.34 | CI lane and local matrix | kind v0.32 / node v1.34.8 |
| 1.35 | CI lane and local matrix | kind v0.32 / node v1.35.5 |
| 1.36 | CI lane and local matrix | kind v0.32 / node v1.36.1 |
| 1.37 | Source-built edge lane and local matrix | kind v0.32 node built from v1.37.0 artifacts |

The 1.37 lane builds a node image because kind v0.32 predates an official 1.37
node image. It is scheduled separately from pull-request CI because building the
image is substantially more expensive. The complete integration scenario has
also passed locally against that source-built image.

## Why one client release can span minors

The core client discovers API groups and resources at runtime and represents
unknown custom resources dynamically. Stable Kubernetes HTTP conventions,
`metav1` list/watch envelopes, JSON Patch, Merge Patch, and Server-Side Apply do
not require the client and server to share a minor version. Generated built-in
resource modules will be versioned separately from the protocol/runtime packages
so schema updates do not force controller-runtime forks.

The reflector follows the upstream-required recovery sequence: on `410 Gone`, it
discards its cache, performs a fresh paginated LIST, and resumes from the returned
resource version. See the upstream [API concepts documentation](https://kubernetes.io/docs/reference/using-api/api-concepts/#efficient-detection-of-changes).

## Feature floor

The initial supported range deliberately starts at Kubernetes 1.34. All active
minors support CRD `apiextensions.k8s.io/v1`, watch bookmarks, status
subresources, Leases, and Server-Side Apply. Streaming initial events are exposed
as an opt-in client feature and require `resourceVersionMatch=NotOlderThan`.

Older clusters may work through the generic client but are not supported until a
repeatable compatibility lane proves them.
