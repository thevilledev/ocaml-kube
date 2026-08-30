let initialized = Atomic.make false

let ensure_rng () =
  if Atomic.compare_and_set initialized false true then
    Mirage_crypto_rng_unix.use_default ()
