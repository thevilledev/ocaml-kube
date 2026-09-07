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
  if is_cancelled t then false
  else
    let wake_read, wake_write = Wakeup.create () in
    Unix.set_nonblock wake_write;
    let wake_lock = Mutex.create () in
    let wake_open = ref true in
    let wake () =
      Mutex.lock wake_lock;
      (if !wake_open then
         try ignore (Unix.write_substring wake_write "x" 0 1)
         with Unix.Unix_error _ -> ());
      Mutex.unlock wake_lock
    in
    let unregister = on_cancel t wake in
    Fun.protect
      ~finally:(fun () ->
        unregister ();
        Mutex.lock wake_lock;
        wake_open := false;
        (try Unix.close wake_read with Unix.Unix_error _ -> ());
        (try Unix.close wake_write with Unix.Unix_error _ -> ());
        Mutex.unlock wake_lock)
      (fun () ->
        let deadline = Clock.deadline seconds in
        let rec wait () =
          if is_cancelled t then false
          else
            let remaining = Clock.remaining deadline in
            if remaining <= 0. then true
            else
              try
                let readable, _, _ =
                  Unix.select [ wake_read ] [] [] remaining
                in
                if readable = [] then not (is_cancelled t) else false
              with Unix.Unix_error (Unix.EINTR, _, _) -> wait ()
        in
        wait ())

let with_timeout ~parent seconds fn =
  if (not (Float.is_finite seconds)) || seconds < 0. then
    invalid_arg "Cancel.with_timeout: timeout must be finite and non-negative";
  let child = create () in
  let timed_out = Atomic.make false in
  let unlink = on_cancel parent (fun () -> cancel child) in
  let timer =
    Thread.create
      (fun () ->
        if sleep child seconds then (
          Atomic.set timed_out true;
          cancel child))
      ()
  in
  let result =
    Fun.protect
      ~finally:(fun () ->
        cancel child;
        unlink ();
        Thread.join timer)
      (fun () -> fn child)
  in
  (result, Atomic.get timed_out)
