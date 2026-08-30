external monotonic_now : unit -> float = "ocaml_kube_monotonic_now"

let now = monotonic_now
let deadline seconds = now () +. max 0. seconds
let remaining deadline = max 0. (deadline -. now ())
let elapsed started = max 0. (now () -. started)
