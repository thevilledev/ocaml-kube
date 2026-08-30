type t = {
  cancelled : bool Atomic.t;
  lock : Mutex.t;
  mutable next_id : int;
  mutable callbacks : (int * (unit -> unit)) list;
}

exception Cancelled

let create () =
  {
    cancelled = Atomic.make false;
    lock = Mutex.create ();
    next_id = 0;
    callbacks = [];
  }

let is_cancelled t = Atomic.get t.cancelled
let check t = if is_cancelled t then raise Cancelled

let on_cancel t callback =
  if is_cancelled t then (
    callback ();
    Fun.id)
  else (
    Mutex.lock t.lock;
    if is_cancelled t then (
      Mutex.unlock t.lock;
      callback ();
      Fun.id)
    else
      let id = t.next_id in
      t.next_id <- id + 1;
      t.callbacks <- (id, callback) :: t.callbacks;
      Mutex.unlock t.lock;
      fun () ->
        Mutex.lock t.lock;
        t.callbacks <-
          List.filter (fun (candidate, _) -> candidate <> id) t.callbacks;
        Mutex.unlock t.lock)

let cancel t =
  if Atomic.compare_and_set t.cancelled false true then (
    Mutex.lock t.lock;
    let callbacks = t.callbacks in
    t.callbacks <- [];
    Mutex.unlock t.lock;
    List.iter (fun (_, callback) -> try callback () with _ -> ()) callbacks)

let sleep t seconds =
  let deadline = Unix.gettimeofday () +. max 0. seconds in
  let rec loop () =
    if is_cancelled t then false
    else
      let remaining = deadline -. Unix.gettimeofday () in
      if remaining <= 0. then true
      else (
        Unix.sleepf (min 0.1 remaining);
        loop ())
  in
  loop ()
