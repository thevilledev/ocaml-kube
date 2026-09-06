(lang dune 3.15)
(name kube)
(version 0.1.0)
(generate_opam_files true)
(source (github thevilledev/ocaml-kube))
(license Apache-2.0)
(authors "Ville Vesilehto" "ocaml-kube contributors")
(maintainers "Ville Vesilehto <ville@vesilehto.fi>")

(package
 (name kube)
 (synopsis "Native OCaml Kubernetes client and controller runtime")
 (description
  "A typed Kubernetes API client and controller runtime implemented with native OCaml threads and explicit cancellation.")
 (tags ("kubernetes" "cloud-native" "operator" "controller" "ocaml-5"))
 (depends
  (ocaml (>= 5.1))
  (alcotest :with-test)
  base64
  ca-certs
  conf-zlib
  domain-name
  ipaddr
  mirage-crypto-rng
  ppxlib
  ptime
  (secret (>= 0.1.2))
  tls
  uri
  x509
  yojson))
