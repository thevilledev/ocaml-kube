type deflater
type inflater

external create_deflater : string -> deflater
  = "ocaml_kube_zlib_deflater_create"

external create_inflater : string -> inflater
  = "ocaml_kube_zlib_inflater_create"

external deflate : deflater -> string -> string = "ocaml_kube_zlib_deflate"

external inflate_raw : inflater -> string -> int -> string
  = "ocaml_kube_zlib_inflate"

let inflate inflater ~max_output value =
  if max_output <= 0 then
    invalid_arg "Spdy_zlib.inflate: max_output must be positive";
  inflate_raw inflater value max_output
