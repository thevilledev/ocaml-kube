#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <time.h>
#endif

CAMLprim value ocaml_kube_monotonic_now(value unit)
{
  CAMLparam1(unit);
#ifdef _WIN32
  LARGE_INTEGER counter;
  LARGE_INTEGER frequency;

  if (!QueryPerformanceFrequency(&frequency) ||
      !QueryPerformanceCounter(&counter)) {
    caml_failwith("QueryPerformanceCounter failed");
  }

  CAMLreturn(caml_copy_double(
      (double)counter.QuadPart / (double)frequency.QuadPart));
#else
  struct timespec timestamp;

  if (clock_gettime(CLOCK_MONOTONIC, &timestamp) != 0) {
    caml_failwith("clock_gettime(CLOCK_MONOTONIC) failed");
  }

  CAMLreturn(caml_copy_double(
      (double)timestamp.tv_sec + ((double)timestamp.tv_nsec / 1000000000.0)));
#endif
}
