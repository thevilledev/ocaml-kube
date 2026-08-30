#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <time.h>

CAMLprim value ocaml_kube_monotonic_now(value unit)
{
  CAMLparam1(unit);
  struct timespec timestamp;

  if (clock_gettime(CLOCK_MONOTONIC, &timestamp) != 0) {
    caml_failwith("clock_gettime(CLOCK_MONOTONIC) failed");
  }

  CAMLreturn(caml_copy_double(
      (double)timestamp.tv_sec + ((double)timestamp.tv_nsec / 1000000000.0)));
}
