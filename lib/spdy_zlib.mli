type deflater
type inflater

val create_deflater : string -> deflater
val create_inflater : string -> inflater
val deflate : deflater -> string -> string
val inflate : inflater -> max_output:int -> string -> string
