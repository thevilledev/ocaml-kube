#!/bin/sh
set -eu

repository=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
acceptance_temp=

cleanup() {
  status=$?
  trap - EXIT INT TERM
  if [ -n "$acceptance_temp" ]; then
    case "$acceptance_temp" in
      */ocaml-k8s-scaffold-acceptance.*) rm -rf -- "$acceptance_temp" ;;
      *)
        echo "refusing to remove unexpected path: $acceptance_temp" >&2
        status=1
        ;;
    esac
  fi
  exit "$status"
}

trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

cd "$repository"
opam exec -- dune build @all @install

acceptance_temp=$(mktemp -d "${TMPDIR:-/tmp}/ocaml-k8s-scaffold-acceptance.XXXXXX")
install_lib="$repository/_build/install/default/lib"
if [ -n "${OCAMLPATH:-}" ]; then
  acceptance_ocamlpath="$install_lib:$OCAMLPATH"
else
  acceptance_ocamlpath=$install_lib
fi

build_case() {
  fixture=$1
  project_name=$2
  project="$acceptance_temp/$project_name"
  "$repository/_build/default/scaffold/ocaml_k8s.exe" scaffold \
    --crd "$repository/scaffold/fixtures/real-world/$fixture" \
    --output "$project"
  OCAMLPATH=$acceptance_ocamlpath opam exec -- dune build \
    --root "$project" @all
  echo "scaffold acceptance passed: $fixture"
}

build_case gateway-api-gatewayclass-v1.5.1.yaml gateway-class-operator
build_case keda-scaledobject-v2.20.1.yaml scaled-object-operator
build_case prometheus-servicemonitor-v0.93.0.yaml service-monitor-operator

init_project="$acceptance_temp/widget-operator"
"$repository/_build/default/scaffold/ocaml_k8s.exe" init \
  --output "$init_project" \
  --group example.dev \
  --kind Widget
OCAMLPATH=$acceptance_ocamlpath opam exec -- dune build \
  --root "$init_project" @all @codegen-check
echo "init acceptance passed: type-derived codecs and CRD"
