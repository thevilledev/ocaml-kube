# Release checklist

Before publishing, confirm the repository URL and maintainer contact in
`dune-project`. `kube.opam` is generated and must not be edited directly.

For a release candidate:

1. Replace the Unreleased heading in `CHANGES.md` with the intended semantic
   version and date.
2. Run `opam install . --deps-only --with-test --with-doc` in a clean switch.
3. Run `opam exec -- dune build -p kube @install @runtest @doc` and
   `opam lint kube.opam`.
4. Run `test/integration_kind.sh` through every active Kubernetes-minor CI lane.
5. Build the source archive from a clean, signed tag and install that archive in
   a fresh opam switch; do not validate only the working tree.
6. Inspect the generated API documentation and installed file list.
7. Submit the opam-repository change only after the source archive and checksum
   are immutable.

Publishing is intentionally a separate, explicit maintainer action. The CI
workflows build and test; they do not create tags, GitHub releases, or opam
submissions.
