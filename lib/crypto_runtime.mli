val ensure_rng : unit -> unit
(** Install the operating-system-backed cryptographic RNG exactly once for TLS
    clients and servers in this process. *)
