# Release checklist

Before publishing, confirm the repository URL and maintainer contact in
`dune-project`. `kube.opam` is generated and must not be edited directly.

For a release candidate:

1. Set `(version ...)` in `dune-project` to the intended semantic version. Keep
   the empty Unreleased section in `CHANGES.md` and add a versioned heading with
   the release date below it.
2. Regenerate `kube.opam` with `opam exec -- dune build kube.opam`, then confirm
   that `opam lint kube.opam` passes.
3. Run `opam install . --deps-only --with-test --with-doc` in a clean switch.
4. Run `opam exec -- dune build -p kube @install @runtest @doc
   @codegen-check`, `codegen/update-kubernetes-api.sh --check all`, and
   `git diff --check`.
5. Run `test/integration_kind.sh` through every active Kubernetes-minor CI lane.
6. Build the source archive from a clean, signed tag and install that archive in
   a fresh opam switch; do not validate only the working tree.
7. Inspect the generated API documentation and installed file list, including
   the `ocaml-kube` executable and every public sublibrary.
8. Submit the opam-repository change only after the source archive and checksum
   are immutable.

Publishing is intentionally a separate, explicit maintainer action. The CI
workflows build and test; they do not create tags, GitHub releases, or opam
submissions.
