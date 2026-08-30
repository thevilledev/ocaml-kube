# Testing clients and controllers

The `kube.test` sublibrary provides deterministic tests without changing the
production client above its final transport call. The normal authentication,
rate limiter, request construction, error classification, JSON decoding, watch
parser, reflector, store, queue, and controller code remains active.

Add `kube.test` to the test stanza:

```lisp
(test
 (name test_operator)
 (libraries kube kube.test alcotest))
```

## FIFO scripts

Use a script when request order is part of the contract:

```ocaml
module Test = Kube_test.Transport

let transport =
  Test.scripted
    [
      Test.respond_json ~status:401
        (`Assoc
          [
            ("kind", `String "Status");
            ("reason", `String "Unauthorized");
            ("code", `Int 401);
          ]);
      Test.respond_json (`Assoc [ ("gitVersion", `String "v1.36.1") ]);
    ]

let configuration =
  Kube_test.config
    ~credential:(Kube.Config.Static_token "test-token") ()

let client = Kube_test.client ~configuration transport
```

`Transport.requests` returns arrival-ordered requests after credential
resolution. Assertions can inspect method, encoded target, headers, body,
streaming mode, and the requested response bound. Header lookup is
case-insensitive through `Transport.header`.

Always call `Transport.verify_complete` at the end of a scripted test. It fails
for unused responses and extra requests, preventing a test from silently
accepting a changed call sequence.

## Programmable handlers

A handler is more suitable for concurrent controllers and route-based behavior:

```ocaml
let is_watch (request : Kube_test.Transport.request) =
  Uri.get_query_param (Uri.of_string request.target) "watch" = Some "true"

let transport =
  Kube_test.Transport.create (fun request ->
      if is_watch request then
        Kube_test.Transport.stream ~wait_for_cancel:true []
      else Kube_test.Transport.respond_json list_response)
```

Handlers may run concurrently and must protect their own mutable state. Request
capture performed by the testkit is thread-safe. A `wait_for_cancel` stream
keeps a reflector request alive and returns promptly when its request token is
cancelled, allowing complete controller startup and graceful-shutdown tests.

Successful streamed response chunks are delivered to the production callback
without buffering. Non-streaming bodies and every error body enforce the
client's requested byte bound. Cancellation is checked before and between
chunks.

## Choosing the right test boundary

The injected transport is deliberately not an in-memory reimplementation of a
Kubernetes API server. It cannot prove defaulting, admission, field ownership,
garbage collection, resource-version ordering, Lease behavior, or CRD schema
validation.

Use each layer for what it can prove:

- `kube.test` for request construction, authentication refresh, decoding,
  application reconciliation, error policy, and cancellation;
- the loopback harness in this repository for HTTP framing, connection reuse,
  truncation, and socket behavior; and
- kind integration tests for Kubernetes semantics and compatibility across
  supported server minors.

The kind proof also runs a controller over two explicit namespace routes. It
waits for both initial snapshots, observes an update in the first namespace and
a deletion in the second, and then verifies joined shutdown. Deterministic tests
separately force a 410 in only one namespace and prove that its scoped relist
preserves the other namespace and every secondary index.

The injected `Kube.Client.Transport` API is also public for record/replay and
specialized deployment transports. Implementations must be safe for concurrent
calls and preserve cancellation, response bounds, and successful-streaming
semantics. The client owns its transport; `Client.close` invokes the close hook
exactly once, potentially while requests are still in flight.
