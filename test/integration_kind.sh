#!/bin/sh
set -eu

repository=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
kubeconfig=${KUBECONFIG:-"$repository/kubeconfig.kind"}
operator_log_a="$repository/_build/greeting-operator-a.integration.log"
operator_log_b="$repository/_build/greeting-operator-b.integration.log"
greeting_resource=greetings.demo.ocaml-k8s.dev
multi_namespace_a=ocaml-k8s-multi-a
multi_namespace_b=ocaml-k8s-multi-b
operator_pid_a=
operator_pid_b=
scaffold_pid=
scaffold_temp=
scaffold_log=

stop_operator() {
  pid=$1
  if [ -n "$pid" ]; then
    if kill -0 "$pid" 2>/dev/null; then
      kill -TERM "$pid"
    fi
    if wait "$pid"; then
      operator_status=0
    else
      operator_status=$?
    fi
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
  stop_operator "$operator_pid_a"
  stop_a_status=$?
  stop_operator "$operator_pid_b"
  stop_b_status=$?
  stop_operator "$scaffold_pid"
  scaffold_stop_status=$?
  if [ "$status" -eq 0 ] && [ "$stop_a_status" -ne 0 ]; then
    status=$stop_a_status
  fi
  if [ "$status" -eq 0 ] && [ "$stop_b_status" -ne 0 ]; then
    status=$stop_b_status
  fi
  if [ "$status" -eq 0 ] && [ "$scaffold_stop_status" -ne 0 ]; then
    status=$scaffold_stop_status
  fi
  if [ "$status" -ne 0 ]; then
    if [ -f "$operator_log_a" ]; then
      echo "operator A log:" >&2
      tail -n 200 "$operator_log_a" >&2
    fi
    if [ -f "$operator_log_b" ]; then
      echo "operator B log:" >&2
      tail -n 200 "$operator_log_b" >&2
    fi
    if [ -n "$scaffold_log" ] && [ -f "$scaffold_log" ]; then
      echo "scaffolded operator log:" >&2
      tail -n 200 "$scaffold_log" >&2
    fi
    kubectl --kubeconfig "$kubeconfig" patch "$greeting_resource" \
      hello-ocaml --type=merge -p '{"metadata":{"finalizers":[]}}' \
      >/dev/null 2>&1 || true
    kubectl --kubeconfig "$kubeconfig" delete "$greeting_resource" \
      hello-ocaml --ignore-not-found --wait=false >/dev/null 2>&1 || true
  fi
  kubectl --kubeconfig "$kubeconfig" patch widgets.example.dev \
    widget-sample --namespace default --type=merge \
    -p '{"metadata":{"finalizers":[]}}' >/dev/null 2>&1 || true
  kubectl --kubeconfig "$kubeconfig" delete widgets.example.dev \
    widget-sample --namespace default --ignore-not-found --wait=false \
    >/dev/null 2>&1 || true
  kubectl --kubeconfig "$kubeconfig" delete crd widgets.example.dev \
    --ignore-not-found --wait=false >/dev/null 2>&1 || true
  kubectl --kubeconfig "$kubeconfig" delete namespace \
    "$multi_namespace_a" "$multi_namespace_b" --ignore-not-found \
    --wait=false >/dev/null 2>&1 || true
  if [ -n "$scaffold_temp" ]; then
    case "$scaffold_temp" in
      */ocaml-k8s-scaffold.*) rm -rf -- "$scaffold_temp" ;;
      *)
        echo "refusing to remove unexpected scaffold path: $scaffold_temp" >&2
        status=1
        ;;
    esac
  fi
  exit "$status"
}

trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

run_scaffold_check() {
  scaffold_temp=$(mktemp -d "${TMPDIR:-/tmp}/ocaml-k8s-scaffold.XXXXXX")
  scaffold_project="$scaffold_temp/widget-operator"
  scaffold_log="$scaffold_temp/operator.log"
  "$repository/_build/default/scaffold/ocaml_k8s.exe" scaffold \
    --crd "$repository/scaffold/fixtures/widget-crd.yaml" \
    --output "$scaffold_project"
  install_lib="$repository/_build/install/default/lib"
  if [ -n "${OCAMLPATH:-}" ]; then
    scaffold_ocamlpath="$install_lib:$OCAMLPATH"
  else
    scaffold_ocamlpath=$install_lib
  fi
  OCAMLPATH=$scaffold_ocamlpath opam exec -- dune build \
    --root "$scaffold_project" @all
  kubectl --kubeconfig "$kubeconfig" apply \
    -f "$scaffold_project/deploy/crd.yaml"
  kubectl --kubeconfig "$kubeconfig" wait --for=condition=Established \
    crd/widgets.example.dev --timeout=60s
  kubectl --kubeconfig "$kubeconfig" apply \
    -f "$scaffold_project/deploy/sample.yaml"
  "$scaffold_project/_build/default/bin/main.exe" \
    --kubeconfig "$kubeconfig" --namespace default >"$scaffold_log" 2>&1 &
  scaffold_pid=$!

  attempts=0
  while [ "$attempts" -lt 30 ]; do
    installed_finalizer=$(kubectl --kubeconfig "$kubeconfig" get \
      widgets.example.dev widget-sample --namespace default \
      -o jsonpath='{.metadata.finalizers[0]}' 2>/dev/null || true)
    if [ "$installed_finalizer" = "widgets.example.dev/finalizer" ]; then
      break
    fi
    attempts=$((attempts + 1))
    sleep 1
  done
  if [ "$installed_finalizer" != "widgets.example.dev/finalizer" ]; then
    echo "scaffolded controller did not install its finalizer" >&2
    return 1
  fi

  kubectl --kubeconfig "$kubeconfig" delete widgets.example.dev \
    widget-sample --namespace default --wait=true --timeout=30s
  stop_operator "$scaffold_pid"
  scaffold_pid=
  kubectl --kubeconfig "$kubeconfig" delete crd widgets.example.dev \
    --wait=true --timeout=30s
  case "$scaffold_temp" in
    */ocaml-k8s-scaffold.*) rm -rf -- "$scaffold_temp" ;;
    *)
      echo "refusing to remove unexpected scaffold path: $scaffold_temp" >&2
      return 1
      ;;
  esac
  scaffold_temp=
  scaffold_log=
  echo "scaffold integration passed: generate, external build, typed decode, finalizer, shutdown"
}

cd "$repository"
opam exec -- dune build @all @install
run_scaffold_check
kubectl --kubeconfig "$kubeconfig" apply -f deploy/crd.yaml
kubectl --kubeconfig "$kubeconfig" wait --for=condition=Established \
  crd/greetings.demo.ocaml-k8s.dev --timeout=60s
"$repository/_build/default/examples/discovery_check.exe" \
  --kubeconfig "$kubeconfig"
"$repository/_build/default/examples/client_features_check.exe" \
  --kubeconfig "$kubeconfig"
kubectl --kubeconfig "$kubeconfig" create namespace "$multi_namespace_a" \
  --dry-run=client -o yaml | kubectl --kubeconfig "$kubeconfig" apply -f -
kubectl --kubeconfig "$kubeconfig" create namespace "$multi_namespace_b" \
  --dry-run=client -o yaml | kubectl --kubeconfig "$kubeconfig" apply -f -
"$repository/_build/default/examples/multi_namespace_check.exe" \
  --kubeconfig "$kubeconfig" --namespace-a "$multi_namespace_a" \
  --namespace-b "$multi_namespace_b"
kubectl --kubeconfig "$kubeconfig" delete namespace \
  "$multi_namespace_a" "$multi_namespace_b" --wait=true --timeout=60s
kubectl --kubeconfig "$kubeconfig" patch "$greeting_resource" hello-ocaml \
  --type=merge -p '{"metadata":{"finalizers":[]}}' >/dev/null 2>&1 || true
kubectl --kubeconfig "$kubeconfig" delete "$greeting_resource" hello-ocaml \
  --ignore-not-found --wait=true
kubectl --kubeconfig "$kubeconfig" delete lease kube-greeting-operator \
  --namespace default --ignore-not-found

"$repository/_build/default/examples/greeting_operator.exe" \
  --kubeconfig "$kubeconfig" --workers 2 --leader-elect \
  --leader-election-name kube-greeting-operator --identity candidate-a \
  >"$operator_log_a" 2>&1 &
operator_pid_a=$!

"$repository/_build/default/examples/greeting_operator.exe" \
  --kubeconfig "$kubeconfig" --workers 2 --leader-elect \
  --leader-election-name kube-greeting-operator --identity candidate-b \
  >"$operator_log_b" 2>&1 &
operator_pid_b=$!

wait_for_holder() {
  previous=$1
  attempts=0
  while [ "$attempts" -lt 60 ]; do
    holder=$(kubectl --kubeconfig "$kubeconfig" get lease \
      kube-greeting-operator --namespace default \
      -o jsonpath='{.spec.holderIdentity}' 2>/dev/null || true)
    if [ -n "$holder" ] && [ "$holder" != "$previous" ]; then
      printf '%s\n' "$holder"
      return 0
    fi
    attempts=$((attempts + 1))
    sleep 1
  done
  echo "timed out waiting for a new leader after '$previous'" >&2
  return 1
}

wait_for_event() {
  uid=$1
  expected_reason=$2
  attempts=0
  jsonpath="{range .items[?(@.regarding.uid==\"$uid\")]}{.reason}{\" \"}{end}"
  while [ "$attempts" -lt 30 ]; do
    reasons=$(kubectl --kubeconfig "$kubeconfig" get events.events.k8s.io \
      --namespace default -o "jsonpath=$jsonpath" 2>/dev/null || true)
    case " $reasons " in
      *" $expected_reason "*) return 0 ;;
    esac
    attempts=$((attempts + 1))
    sleep 1
  done
  echo "timed out waiting for Event reason $expected_reason for UID $uid" >&2
  return 1
}

first_holder=$(wait_for_holder "")

kubectl --kubeconfig "$kubeconfig" apply -f deploy/sample.yaml
kubectl --kubeconfig "$kubeconfig" wait \
  --for=jsonpath='{.status.reconciledMessage}'='hello from OCaml' \
  "$greeting_resource/hello-ocaml" --timeout=30s
greeting_uid=$(kubectl --kubeconfig "$kubeconfig" get \
  "$greeting_resource" hello-ocaml -o jsonpath='{.metadata.uid}')
wait_for_event "$greeting_uid" Reconciled

case "$first_holder" in
  candidate-a)
    stop_operator "$operator_pid_a"
    operator_pid_a=
    ;;
  candidate-b)
    stop_operator "$operator_pid_b"
    operator_pid_b=
    ;;
  *)
    echo "unexpected first leader: $first_holder" >&2
    exit 1
    ;;
esac

second_holder=$(wait_for_holder "$first_holder")

kubectl --kubeconfig "$kubeconfig" patch "$greeting_resource" hello-ocaml --type=merge \
  -p '{"spec":{"message":"updated by watch"}}'
kubectl --kubeconfig "$kubeconfig" wait \
  --for=jsonpath='{.status.reconciledMessage}'='updated by watch' \
  "$greeting_resource/hello-ocaml" --timeout=30s

observed=$(kubectl --kubeconfig "$kubeconfig" get "$greeting_resource" hello-ocaml \
  -o jsonpath='{.metadata.generation}:{.status.observedGeneration}:{.status.reconciledMessage}:{.status.phase.type}')
test "$observed" = "2:2:updated by watch:Ready"

kubectl --kubeconfig "$kubeconfig" delete "$greeting_resource" hello-ocaml \
  --wait=true --timeout=30s
if kubectl --kubeconfig "$kubeconfig" get "$greeting_resource" hello-ocaml >/dev/null 2>&1; then
  echo "resource still exists after finalization" >&2
  exit 1
fi
wait_for_event "$greeting_uid" Finalized

stop_operator "$operator_pid_a"
operator_pid_a=
stop_operator "$operator_pid_b"
operator_pid_b=

grep -q 'generation 1: hello from OCaml' "$operator_log_a" "$operator_log_b"
grep -q 'generation 2: updated by watch' "$operator_log_a" "$operator_log_b"
grep -q 'finalized default/hello-ocaml' "$operator_log_a" "$operator_log_b"
grep -q "acquired leadership as $first_holder" "$operator_log_a" "$operator_log_b"
grep -q "acquired leadership as $second_holder" "$operator_log_a" "$operator_log_b"

kubectl --kubeconfig "$kubeconfig" delete lease kube-greeting-operator \
  --namespace default --ignore-not-found

echo "kind integration passed: scaffold generation/build/run, discovery, collection delete, subresources, Scale, logs, multi-namespace cache, leader contention, failover, watch update, status, events, finalizer, shutdown"
