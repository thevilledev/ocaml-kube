type bucket = {
  qps : float;
  burst : float;
  mutex : Mutex.t;
  mutable tokens : float;
  mutable updated_at : float;
}

type t = Unlimited | Bucket of bucket

let finite_positive value =
  value > 0.0
  &&
  match classify_float value with
  | FP_normal | FP_subnormal -> true
  | FP_zero | FP_infinite | FP_nan -> false

let create ~qps ~burst =
  if not (finite_positive qps) then
    invalid_arg "Rate_limiter.create: qps must be finite and positive";
  if burst < 1 then invalid_arg "Rate_limiter.create: burst must be positive";
  Bucket
    {
      qps;
      burst = float_of_int burst;
      mutex = Mutex.create ();
      tokens = float_of_int burst;
      updated_at = Clock.now ();
    }

let unlimited = Unlimited

let acquire ?cancel = function
  | Unlimited -> not (Option.fold ~none:false ~some:Cancel.is_cancelled cancel)
  | Bucket bucket ->
      let cancel = Option.value ~default:(Cancel.create ()) cancel in
      let rec wait () =
        if Cancel.is_cancelled cancel then false
        else (
          Mutex.lock bucket.mutex;
          let now = Clock.now () in
          let elapsed = max 0.0 (now -. bucket.updated_at) in
          bucket.tokens <-
            min bucket.burst (bucket.tokens +. (elapsed *. bucket.qps));
          bucket.updated_at <- max bucket.updated_at now;
          let delay =
            if bucket.tokens >= 1.0 then (
              bucket.tokens <- bucket.tokens -. 1.0;
              None)
            else Some ((1.0 -. bucket.tokens) /. bucket.qps)
          in
          Mutex.unlock bucket.mutex;
          match delay with
          | None -> true
          | Some delay -> if Cancel.sleep cancel delay then wait () else false)
      in
      wait ()
