#!/bin/sh
set -eu

repository=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
versions="$repository/codegen/kubernetes-versions.tsv"
mode=write
selection=all

usage() {
  echo "usage: codegen/update-kubernetes-api.sh [--check] [all|MINOR]" >&2
  exit 2
}

if [ "${1:-}" = "--check" ]; then
  mode=check
  shift
fi
if [ "$#" -gt 1 ]; then
  usage
fi
if [ "$#" -eq 1 ]; then
  selection=$1
fi

temporary=$(mktemp -d "${TMPDIR:-/tmp}/ocaml-kube-openapi.XXXXXX")
trap 'rm -rf "$temporary"' EXIT HUP INT TERM
matched=false

render_dune() {
  dune_destination=$1
  dune_suffix=$2
  dune_patch=$3
  cat >"$dune_destination" <<EOF
(library
 (name kube_api_${dune_suffix})
 (public_name kube.api.${dune_suffix})
 (libraries kube yojson))

(rule
 (alias codegen-check)
 (deps
  ../../codegen/resources-v${dune_patch}.json
  ../../codegen/openapi/v${dune_patch}.json
  kube_api_${dune_suffix}.ml
  kube_api_${dune_suffix}.mli)
 (action
  (run
   %{exe:../../codegen/kube_codegen.exe}
   --schema
   ../../codegen/openapi/v${dune_patch}.json
   --manifest
   ../../codegen/resources-v${dune_patch}.json
   --ml
   kube_api_${dune_suffix}.ml
   --mli
   kube_api_${dune_suffix}.mli
   --check)))
EOF
}

while read -r minor patch sha256; do
  case "$minor" in
    ''|'#'*) continue ;;
  esac
  if [ "$selection" != all ] && [ "$selection" != "$minor" ]; then
    continue
  fi
  matched=true
  tag="v$patch"
  suffix="v$(printf '%s' "$minor" | tr . _)"
  source_url="https://raw.githubusercontent.com/kubernetes/kubernetes/$tag/api/openapi-spec/swagger.json"
  download="$temporary/$tag.json"
  manifest="$repository/codegen/resources-$tag.json"
  schema="$repository/codegen/openapi/$tag.json"
  package_directory="$repository/api/$suffix"
  implementation="$package_directory/kube_api_${suffix}.ml"
  interface="$package_directory/kube_api_${suffix}.mli"
  expected_dune="$temporary/dune-$suffix"

  curl --fail --silent --show-error --location "$source_url" --output "$download"
  if command -v sha256sum >/dev/null 2>&1; then
    actual_sha256=$(sha256sum "$download" | cut -d ' ' -f 1)
  else
    actual_sha256=$(shasum -a 256 "$download" | cut -d ' ' -f 1)
  fi
  if [ "$actual_sha256" != "$sha256" ]; then
    echo "$tag OpenAPI checksum mismatch: expected $sha256, got $actual_sha256" >&2
    exit 1
  fi

  render_dune "$expected_dune" "$suffix" "$patch"
  if [ "$mode" = write ]; then
    mkdir -p "$package_directory" "$repository/codegen/openapi"
    cp "$expected_dune" "$package_directory/dune"
    codegen_mode=
  else
    if ! cmp -s "$expected_dune" "$package_directory/dune"; then
      echo "$package_directory/dune is not synchronized with $versions" >&2
      exit 1
    fi
    codegen_mode=--check
  fi

  cd "$repository"
  opam exec -- dune exec codegen/kube_codegen.exe -- \
    --schema "$download" \
    --manifest "$manifest" \
    --derive-stable-resources \
    --kubernetes-version "$tag" \
    --source "$source_url" \
    --sha256 "$sha256" \
    --ml "$implementation" \
    --mli "$interface" \
    --schema-output "$schema" \
    $codegen_mode
done <"$versions"

if [ "$matched" != true ]; then
  echo "Kubernetes minor $selection is not listed in $versions" >&2
  exit 2
fi

opam exec -- dune build @codegen-check
