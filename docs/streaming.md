# Streaming subresources

`ocaml-kube` implements Kubernetes upgraded connections in the core client, so
exec, attach, and port forwarding inherit ordinary authentication, TLS,
kubeconfig, proxy, impersonation, rate limiting, and cancellation behavior.
Client shutdown closes every active upgraded connection.

## Exec and attach

`Kube.Remote_command.exec` and `attach` return an explicit session. Exec accepts
an ordered command argument list; both operations select container, namespace,
stdin, stdout, stderr, and TTY streams independently. TTY mode disables the
separate stderr channel, matching Kubernetes behavior.

The default negotiation requests stream protocol V5. The implementation also
decodes V1 through V4 for older servers. V3 adds terminal resize, V4 adds JSON
Status exit messages, and V5 adds per-stream close messages. Use:

- `send_stdin` to write a binary stdin chunk;
- `close_stdin` for a V5 half-close;
- `resize` for a TTY width/height update;
- `receive` for stdout, stderr, stream-close, exit, remote-error, and connection
  events; and
- `close` to terminate the entire session.

Only one caller should invoke `receive`; writes are serialized and may run
concurrently. Pass a `Cancel.t` to interrupt a blocked receive and bound the
session lifetime.

## Port forwarding

`Kube.Port_forward.connect` opens one multiplexed connection for a Pod.
`open_stream` creates the Kubernetes error/data pair for one remote port.
`read`, `write`, `close_write`, and `close_stream` provide a socket-like API.

`Kube.Port_forward.Forwarder.start` is the higher-level local forwarder. It
accepts mappings such as:

```ocaml
let ports =
  Kube.Port_forward.Forwarder.
    [ { local_port = 8443; remote_port = 443 } ]

let forwarder =
  Kube.Port_forward.Forwarder.start client ~pod:"api" ~ports ()
```

Local port zero asks the OS to choose a free port; inspect the result with
`bound_ports`. Listen addresses must be explicit IP literals and default to
`127.0.0.1`, avoiding accidental public exposure. Multiple listeners and
multiple accepted TCP connections share one API-server tunnel. `await` blocks
until cancellation, explicit closure, or fatal tunnel/listener failure; `close`
joins listener and connection workers.

The tunnel uses Kubernetes' current WebSocket port-forward subprotocol and its
continuous SPDY/3.1 byte stream. Header compression is stateful and uses the
standard preset dictionary. Each logical receive queue defaults to 4 MiB and
resets on overflow; WebSocket messages and SPDY frames are independently
bounded. Local reads and writes use 16 KiB chunks, preventing whole-connection
buffering.

## Verification

The loopback suite checks upgrade authentication, masking, fragmentation,
ping/pong, exit Status decoding, stream headers, compression state, half-close,
and local TCP forwarding. The kind integration creates a two-container Pod and
proves exec output, attach stdin echo, and HTTP traffic through a forwarded
port against a real API server and kubelet.
