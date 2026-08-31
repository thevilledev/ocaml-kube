#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <stdint.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <zlib.h>

enum ocaml_kube_zlib_kind {
  OCAML_KUBE_DEFLATER = 1,
  OCAML_KUBE_INFLATER = 2
};

struct ocaml_kube_zlib_state {
  z_stream stream;
  enum ocaml_kube_zlib_kind kind;
  unsigned char *dictionary;
  uInt dictionary_length;
  int initialized;
};

#define Zlib_state_val(value) \
  ((struct ocaml_kube_zlib_state *)Data_custom_val(value))

static void ocaml_kube_zlib_finalize(value wrapped)
{
  struct ocaml_kube_zlib_state *state = Zlib_state_val(wrapped);
  if (state->initialized) {
    if (state->kind == OCAML_KUBE_DEFLATER) {
      deflateEnd(&state->stream);
    } else {
      inflateEnd(&state->stream);
    }
    state->initialized = 0;
  }
  free(state->dictionary);
  state->dictionary = NULL;
  state->dictionary_length = 0;
}

static struct custom_operations ocaml_kube_zlib_operations = {
  "ocaml-kube.spdy-zlib-state",
  ocaml_kube_zlib_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static value ocaml_kube_zlib_create(value dictionary,
                                     enum ocaml_kube_zlib_kind kind)
{
  CAMLparam1(dictionary);
  CAMLlocal1(wrapped);
  mlsize_t dictionary_length = caml_string_length(dictionary);
  if (dictionary_length > UINT_MAX) {
    caml_invalid_argument("SPDY dictionary is too large");
  }
  wrapped = caml_alloc_custom(&ocaml_kube_zlib_operations,
                              sizeof(struct ocaml_kube_zlib_state), 0, 1);
  struct ocaml_kube_zlib_state *state = Zlib_state_val(wrapped);
  memset(state, 0, sizeof(*state));
  state->kind = kind;
  state->dictionary_length = (uInt)dictionary_length;
  state->dictionary = malloc(dictionary_length == 0 ? 1 : dictionary_length);
  if (state->dictionary == NULL) {
    caml_raise_out_of_memory();
  }
  memcpy(state->dictionary, String_val(dictionary), dictionary_length);

  int result;
  if (kind == OCAML_KUBE_DEFLATER) {
    result = deflateInit(&state->stream, Z_BEST_COMPRESSION);
    if (result == Z_OK) {
      state->initialized = 1;
      result = deflateSetDictionary(&state->stream, state->dictionary,
                                    state->dictionary_length);
    }
  } else {
    result = inflateInit(&state->stream);
    if (result == Z_OK) {
      state->initialized = 1;
    }
  }
  if (result != Z_OK) {
    ocaml_kube_zlib_finalize(wrapped);
    caml_failwith(kind == OCAML_KUBE_DEFLATER
                      ? "failed to initialize SPDY zlib deflater"
                      : "failed to initialize SPDY zlib inflater");
  }
  CAMLreturn(wrapped);
}

CAMLprim value ocaml_kube_zlib_deflater_create(value dictionary)
{
  return ocaml_kube_zlib_create(dictionary, OCAML_KUBE_DEFLATER);
}

CAMLprim value ocaml_kube_zlib_inflater_create(value dictionary)
{
  return ocaml_kube_zlib_create(dictionary, OCAML_KUBE_INFLATER);
}

CAMLprim value ocaml_kube_zlib_deflate(value wrapped, value input)
{
  CAMLparam2(wrapped, input);
  CAMLlocal1(output);
  struct ocaml_kube_zlib_state *state = Zlib_state_val(wrapped);
  if (!state->initialized || state->kind != OCAML_KUBE_DEFLATER) {
    caml_invalid_argument("invalid SPDY zlib deflater");
  }
  mlsize_t input_length = caml_string_length(input);
  if (input_length > UINT_MAX) {
    caml_invalid_argument("SPDY header block is too large");
  }
  size_t capacity = (size_t)deflateBound(&state->stream, (uLong)input_length) + 16;
  if (capacity < 64) capacity = 64;
  unsigned char *buffer = malloc(capacity);
  if (buffer == NULL) caml_raise_out_of_memory();

  state->stream.next_in = (Bytef *)String_val(input);
  state->stream.avail_in = (uInt)input_length;
  size_t produced = 0;
  int result = Z_OK;
  do {
    if (produced == capacity) {
      size_t next_capacity = capacity * 2;
      unsigned char *next = realloc(buffer, next_capacity);
      if (next == NULL) {
        free(buffer);
        caml_raise_out_of_memory();
      }
      buffer = next;
      capacity = next_capacity;
    }
    state->stream.next_out = buffer + produced;
    state->stream.avail_out = (uInt)(capacity - produced > UINT_MAX
                                         ? UINT_MAX
                                         : capacity - produced);
    uInt available = state->stream.avail_out;
    result = deflate(&state->stream, Z_SYNC_FLUSH);
    produced += (size_t)(available - state->stream.avail_out);
  } while (result == Z_OK &&
           (state->stream.avail_in > 0 || state->stream.avail_out == 0));

  if (result != Z_OK) {
    free(buffer);
    caml_failwith("SPDY zlib deflate failed");
  }
  output = caml_alloc_initialized_string(produced, (const char *)buffer);
  free(buffer);
  CAMLreturn(output);
}

CAMLprim value ocaml_kube_zlib_inflate(value wrapped, value input,
                                       value maximum)
{
  CAMLparam3(wrapped, input, maximum);
  CAMLlocal1(output);
  struct ocaml_kube_zlib_state *state = Zlib_state_val(wrapped);
  if (!state->initialized || state->kind != OCAML_KUBE_INFLATER) {
    caml_invalid_argument("invalid SPDY zlib inflater");
  }
  intnat maximum_output = Long_val(maximum);
  if (maximum_output <= 0) {
    caml_invalid_argument("SPDY inflate bound must be positive");
  }
  mlsize_t input_length = caml_string_length(input);
  if (input_length > UINT_MAX) {
    caml_invalid_argument("compressed SPDY header block is too large");
  }
  size_t capacity = (size_t)maximum_output < 4096
                        ? (size_t)maximum_output
                        : 4096;
  unsigned char *buffer = malloc(capacity);
  if (buffer == NULL) caml_raise_out_of_memory();

  state->stream.next_in = (Bytef *)String_val(input);
  state->stream.avail_in = (uInt)input_length;
  size_t produced = 0;
  int dictionary_set = 0;
  for (;;) {
    if (produced == capacity) {
      if (capacity >= (size_t)maximum_output) {
        free(buffer);
        caml_failwith("SPDY decompressed header block exceeds configured limit");
      }
      size_t next_capacity = capacity * 2;
      if (next_capacity > (size_t)maximum_output)
        next_capacity = (size_t)maximum_output;
      unsigned char *next = realloc(buffer, next_capacity);
      if (next == NULL) {
        free(buffer);
        caml_raise_out_of_memory();
      }
      buffer = next;
      capacity = next_capacity;
    }
    state->stream.next_out = buffer + produced;
    state->stream.avail_out = (uInt)(capacity - produced > UINT_MAX
                                         ? UINT_MAX
                                         : capacity - produced);
    uInt available = state->stream.avail_out;
    int result = inflate(&state->stream, Z_SYNC_FLUSH);
    produced += (size_t)(available - state->stream.avail_out);
    if (result == Z_NEED_DICT && !dictionary_set) {
      if (inflateSetDictionary(&state->stream, state->dictionary,
                               state->dictionary_length) != Z_OK) {
        free(buffer);
        caml_failwith("SPDY zlib dictionary does not match peer");
      }
      dictionary_set = 1;
      continue;
    }
    if (result != Z_OK && result != Z_BUF_ERROR && result != Z_STREAM_END) {
      free(buffer);
      caml_failwith("SPDY zlib inflate failed");
    }
    if (state->stream.avail_in == 0 && state->stream.avail_out > 0) break;
    if (result == Z_BUF_ERROR && state->stream.avail_in == 0) break;
  }

  output = caml_alloc_initialized_string(produced, (const char *)buffer);
  free(buffer);
  CAMLreturn(output);
}
