#!/bin/sh
set -eu

repository=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
kubeconfig=${KUBECONFIG:-"$repository/kubeconfig.kind"}
operator_log="$repository/_build/greeting-operator.integration.log"
operator_pid=

stop_operator() {
  if [ -n "$operator_pid" ] && kill -0 "$operator_pid" 2>/dev/null; then
    kill -TERM "$operator_pid"
    set +e
    wait "$operator_pid"
    operator_status=$?
    set -e
    if [ "$operator_status" -ne 0 ]; then
      echo "operator exited with status $operator_status during shutdown" >&2
      return "$operator_status"
    fi
  fi
}

cleanup() {
  status=$?
  trap - EXIT INT TERM
  set +e
  stop_operator
  stop_status=$?
  if [ "$status" -eq 0 ] && [ "$stop_status" -ne 0 ]; then
    status=$stop_status
  fi
  if [ "$status" -ne 0 ] && [ -f "$operator_log" ]; then
    echo "operator log:" >&2
    tail -n 200 "$operator_log" >&2
  fi
  exit "$status"
}

trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

cd "$repository"
opam exec -- dune build @all
kubectl --kubeconfig "$kubeconfig" apply -f deploy/crd.yaml
kubectl --kubeconfig "$kubeconfig" wait --for=condition=Established \
  crd/greetings.demo.kube-ocaml.dev --timeout=60s

"$repository/_build/default/examples/greeting_operator.exe" \
  --kubeconfig "$kubeconfig" --workers 2 >"$operator_log" 2>&1 &
operator_pid=$!

kubectl --kubeconfig "$kubeconfig" apply -f deploy/sample.yaml
kubectl --kubeconfig "$kubeconfig" wait \
  --for=jsonpath='{.status.reconciledMessage}'='hello from OCaml' \
  greeting/hello-ocaml --timeout=30s

kubectl --kubeconfig "$kubeconfig" patch greeting hello-ocaml --type=merge \
  -p '{"spec":{"message":"updated by watch"}}'
kubectl --kubeconfig "$kubeconfig" wait \
  --for=jsonpath='{.status.reconciledMessage}'='updated by watch' \
  greeting/hello-ocaml --timeout=30s

observed=$(kubectl --kubeconfig "$kubeconfig" get greeting hello-ocaml \
  -o jsonpath='{.metadata.generation}:{.status.observedGeneration}:{.status.reconciledMessage}')
test "$observed" = "2:2:updated by watch"

kubectl --kubeconfig "$kubeconfig" delete greeting hello-ocaml \
  --wait=true --timeout=30s
if kubectl --kubeconfig "$kubeconfig" get greeting hello-ocaml >/dev/null 2>&1; then
  echo "resource still exists after finalization" >&2
  exit 1
fi

stop_operator
operator_pid=

grep -q 'generation 1: hello from OCaml' "$operator_log"
grep -q 'generation 2: updated by watch' "$operator_log"
grep -q 'finalized default/hello-ocaml' "$operator_log"

echo "kind integration passed: create, watch update, status, finalizer, shutdown"
