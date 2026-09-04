type backoff = {
  initial : float;
  maximum : float;
  factor : float;
  jitter : float;
  max_attempts : int;
}

let exponential ?(initial = 0.01) ?(maximum = 1.) ?(factor = 2.) ?(jitter = 0.1)
    ~max_attempts () =
  let finite_non_negative name value =
    if (not (Float.is_finite value)) || value < 0. then
      invalid_arg
        ("Retry.exponential: " ^ name ^ " must be finite and non-negative")
  in
  finite_non_negative "initial" initial;
  finite_non_negative "maximum" maximum;
  finite_non_negative "factor" factor;
  finite_non_negative "jitter" jitter;
  if factor < 1. then
    invalid_arg "Retry.exponential: factor must be at least one";
  if max_attempts < 1 then
    invalid_arg "Retry.exponential: max_attempts must be positive";
  { initial; maximum; factor; jitter; max_attempts }

let default_conflict =
  exponential ~initial:0.01 ~maximum:0.05 ~factor:1. ~jitter:0.1 ~max_attempts:5
    ()

let on_error ?cancel ?(backoff = default_conflict) ~retry operation =
  let cancel = Option.value ~default:(Cancel.create ()) cancel in
  let random = Random.State.make_self_init () in
  let rec run attempt delay =
    match operation () with
    | Ok _ as result -> result
    | Error error as result ->
        if attempt >= backoff.max_attempts || not (retry error) then result
        else
          let jittered =
            delay *. (1. +. (backoff.jitter *. Random.State.float random 1.))
          in
          let requested =
            Option.value ~default:0. (Client.Error.suggested_delay error)
          in
          let wait = max requested (min backoff.maximum jittered) in
          if Cancel.sleep cancel wait then
            run (attempt + 1) (min backoff.maximum (delay *. backoff.factor))
          else result
  in
  run 1 backoff.initial

let on_conflict ?cancel ?backoff operation =
  on_error ?cancel ?backoff ~retry:Client.Error.is_conflict operation
