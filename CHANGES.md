# Changelog

All notable changes will be recorded here. The project follows Semantic
Versioning once the first release is tagged.

## Unreleased

### Added

- Native OCaml 5 Kubernetes HTTP and TLS transport.
- Kubeconfig, in-cluster, token-file, basic, client-certificate, and exec-plugin
  authentication.
- Typed and dynamic CRUD, patch, Server-Side Apply, pagination, discovery,
  status, arbitrary subresources, and watch support.
- Resource-version-aware reflectors, local stores, deduplicating work queues,
  delayed requeue, exponential retry, finalizer helpers, and concurrent
  controllers.
- A typed custom-resource operator and repeatable kind integration test.
- Compatibility lanes for active Kubernetes minor releases.
- Kubeconfig merging and client-go-compatible exec-plugin validation, relative
  command resolution, and token-file precedence.

### Security

- Bound buffered HTTP response bodies and individual watch frames.
- Reject newline injection in request targets and headers.
- Refresh cached exec credentials once after an HTTP 401 response.
- Validate HTTP header grammar, reserve framing headers to the transport, and
  reject unencoded request-target whitespace.

### Fixed

- Keep polling and draining exec credential helpers until both their process and
  output pipes complete, instead of returning after an initial nonblocking poll.
- Omit unknown resource versions from finalizer patches instead of sending JSON
  null.
