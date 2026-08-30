#!/bin/sh
set -eu

repository=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
manifest="$repository/codegen/resources-v1.36.2.json"
schema="$repository/codegen/openapi/v1.36.2.json"
implementation="$repository/api/v1_36/kube_api_v1_36.ml"
interface="$repository/api/v1_36/kube_api_v1_36.mli"
source_url="https://raw.githubusercontent.com/kubernetes/kubernetes/v1.36.2/api/openapi-spec/swagger.json"
expected_sha256="dcede2063da1d7ad62ecb5af8adb6d7fabd0b52385a7fa0048afb491dac90450"
download=$(mktemp "${TMPDIR:-/tmp}/ocaml-k8s-openapi.XXXXXX")
trap 'rm -f "$download"' EXIT HUP INT TERM

curl --fail --silent --show-error --location "$source_url" --output "$download"

if command -v sha256sum >/dev/null 2>&1; then
  actual_sha256=$(sha256sum "$download" | cut -d ' ' -f 1)
else
  actual_sha256=$(shasum -a 256 "$download" | cut -d ' ' -f 1)
fi

if [ "$actual_sha256" != "$expected_sha256" ]; then
  echo "OpenAPI checksum mismatch: expected $expected_sha256, got $actual_sha256" >&2
  exit 1
fi

cd "$repository"
opam exec -- dune exec codegen/kube_codegen.exe -- \
  --schema "$download" \
  --manifest "$manifest" \
  --derive-stable-resources \
  --ml "$implementation" \
  --mli "$interface" \
  --schema-output "$schema"

opam exec -- dune build @codegen-check
