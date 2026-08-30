(lang dune 3.15)
(name kube)
(generate_opam_files true)
(source (github thevilledev/ocaml-k8s))
(license Apache-2.0)
(authors "Ville Vesilehto" "ocaml-k8s contributors")
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
  tls
  uri
  x509
  yojson))
