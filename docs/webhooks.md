# Admission and conversion webhooks

`ocaml-kube` implements the Kubernetes `admission.k8s.io/v1` and
`apiextensions.k8s.io/v1` review protocols and provides a supervised HTTPS
server for them.

## Typed admission handlers

`Admission.For` checks the request GVK before decoding current and previous
objects with the resource module:

```ocaml
module Greeting_admission = Kube.Admission.For (Greeting)

let validate =
  Greeting_admission.handler (fun ~cancel:_ request ->
      match request.object_ with
      | Some greeting when String.trim greeting.spec.message <> "" ->
          Ok (Kube.Admission.allow ())
      | Some _ ->
          Ok
            (Kube.Admission.deny ~code:422 ~reason:"Invalid"
               "spec.message must not be empty")
      | None ->
          Ok
            (Kube.Admission.deny ~code:400 ~reason:"BadRequest"
               "request has no object"))
```

A mutating handler returns RFC 6902 operations. The server base64-encodes the
patch and sets `patchType: JSONPatch`:

```ocaml
let default_label ~cancel:_ _request =
  Ok
    (Kube.Admission.patch
       [
         Kube.Admission.Add
           {
             path = "/metadata/labels/managed-by";
             value = `String "ocaml-kube";
           };
       ])
```

Handlers can attach Kubernetes warning strings and audit annotations through
the optional arguments to `allow`, `deny`, and `patch`. A returned error or
uncaught handler exception becomes a UID-preserving denied AdmissionResponse
with code 500. Malformed reviews are rejected at the HTTP boundary because
their UID is not trusted.

## TLS server and manager integration

The server accepts PEM material directly, which makes the ownership of
certificate provisioning explicit:

```ocaml
let webhook =
  Kube.Webhook.create ~address:"0.0.0.0" ~port:9443
    ~certificate_pem ~private_key_pem ~metrics ()
  |> Result.get_ok

let () =
  Kube.Webhook.add_admission webhook ~path:"/validate-greeting" validate;
  Kube.Webhook.add_admission webhook ~path:"/mutate-greeting" default_label;
  Kube.Health.add_readiness health ~name:"webhook"
    (Kube.Webhook.readiness_check webhook)
  |> ignore;
  Kube.Manager.add manager (Kube.Webhook.component webhook)
```

The server:

- supports TLS 1.2 and TLS 1.3;
- can require client certificates with `~client_ca_pem`;
- bounds concurrent connections, headers, and bodies;
- rejects ambiguous `Content-Length`, transfer encoding, content encoding,
  folded headers, and unsupported content types;
- gives TLS handshake, request body, and handler execution one monotonic
  cancellation deadline;
- closes timed-out connections and passes the linked cancellation token into
  handlers;
- emits request, failure, and latency metrics for every configured path; and
- stops accepting work, cancels active requests, and joins every connection
  before its manager component returns.

Handler cancellation is cooperative. A blocking handler must pass its token to
client calls and cancellation-aware waits.

The process currently loads certificate material when `Webhook.create` is
called. Deployments that rotate a mounted Secret must recreate the server or
restart the process. Automatic certificate issuance, Secret watching, and
WebhookConfiguration `caBundle` management remain deployment concerns.

## CRD conversion

`Conversion.map` lifts a one-object conversion function across a
ConversionRequest:

```ocaml
let convert =
  Kube.Conversion.map
    (fun ~cancel:_ ~desired_api_version object_ ->
      match object_ with
      | `Assoc fields ->
          Ok
            (`Assoc
               (("apiVersion", `String desired_api_version)
               :: List.remove_assoc "apiVersion" fields))
      | _ -> Error "custom resource must be an object")

let () =
  Kube.Webhook.add_conversion webhook ~path:"/convert-greeting" convert
```

Successful output is checked before it reaches the API server:

- the output count must equal the input count;
- object ordering is preserved by `Conversion.map`;
- every `apiVersion` must equal `desiredAPIVersion`; and
- kind, name, namespace, and UID must remain unchanged.

Conversion errors and exceptions produce a protocol-valid failure
ConversionResponse with the original request UID.

## Kubernetes configuration

The serving certificate must contain the webhook Service DNS name, and the
matching CA must be placed in the admission or CRD conversion configuration's
`caBundle`. RBAC does not authorize webhook calls; reachability and TLS identity
are the trust boundary. Keep validating and mutating failure policies explicit,
set realistic API-server timeouts, and make handlers deterministic and
idempotent.
