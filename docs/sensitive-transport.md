# Sensitive transport

`Http.Sensitive` and `Client.Sensitive` provide a deliberately separate path for
credentials and payloads held in [`Secret.t`](https://opam.ocaml.org/packages/secret/).
The ordinary string APIs remain source-compatible, but do not acquire these
memory properties.

## Boundary

The sensitive path:

- reads `Token_file` credentials directly through `Secret_unix`;
- assembles the `Bearer ` prefix and token in a temporary protected value;
- writes public headers, protected header values, and a protected request body
  as separate segments rather than concatenating them in an OCaml `Buffer`;
- reads bounded success and error bodies directly into `Secret.t`;
- serializes Secret data to base64 and JSON directly into a temporary protected
  request buffer; and
- destroys temporary authorization, request, response, and decoding buffers on
  every synchronous return or exception path.

`Client.Sensitive.raw` defaults to `strict_credentials=true`. Strict mode
rejects inline bearer tokens, basic authentication, exec-plugin output, client
private keys, and authenticated proxy URLs because those values have already
existed in the OCaml heap. Set it to `false` only for compatibility or
development; this does not retroactively protect those source values.

The returned `Http.Sensitive.response.body` and the token returned by
`create_service_account_token` are owned by the caller and must be destroyed.
Request bodies and sensitive headers are borrowed synchronously and must remain
alive until the call returns.

## Secret writes without reads

`Client.Sensitive.create_secret` accepts `(string * Secret.t) list` data and
returns only the non-sensitive resource version on success. `patch_secret`
emits an RFC 6902 patch that first
tests a caller-selected source annotation and then replaces `/data`. Neither
operation performs or requires a Kubernetes Secret GET.

```ocaml
let value = Secret.of_string "development-only example" in
Fun.protect
  ~finally:(fun () -> Secret.destroy value)
  (fun () ->
    Kube.Client.Sensitive.patch_secret client ~namespace:"default"
      ~name:"managed-secret"
      ~source_annotation:("example.dev/source-uid", source_uid)
      ~data:[ ("password", value) ])
```

## Non-guarantees

This API controls storage owned by `kube` and `secret`. It cannot erase copies
inside TLS implementations, the kernel, Kubernetes API server, remote services,
registers, hardware, a hypervisor, or a privileged observer. Public response
metadata such as status, reason, and headers remains ordinary OCaml data. Do not
place secrets in URLs, header names, status messages, logs, labels, or resource
names.
