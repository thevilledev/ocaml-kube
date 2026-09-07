type state = { mutable dirty : bool; mutable processing : bool }

type t = {
  mutex : Mutex.t;
  ready : Condition.t;
  queue : Core.Object_key.t Queue.t;
  states : (Core.Object_key.t, state) Hashtbl.t;
  mutable closed : bool;
}

let create () =
  {
    mutex = Mutex.create ();
    ready = Condition.create ();
    queue = Queue.create ();
    states = Hashtbl.create 127;
    closed = false;
  }

let add queue key =
  Mutex.lock queue.mutex;
  (if not queue.closed then
     match Hashtbl.find_opt queue.states key with
     | None ->
         Hashtbl.add queue.states key { dirty = true; processing = false };
         Queue.add key queue.queue;
         Condition.signal queue.ready
     | Some state when state.dirty -> ()
     | Some state ->
         state.dirty <- true;
         if not state.processing then (
           Queue.add key queue.queue;
           Condition.signal queue.ready));
  Mutex.unlock queue.mutex

let take queue =
  Mutex.lock queue.mutex;
  while Queue.is_empty queue.queue && not queue.closed do
    Condition.wait queue.ready queue.mutex
  done;
  let result =
    if Queue.is_empty queue.queue then None
    else
      let key = Queue.take queue.queue in
      let state = Hashtbl.find queue.states key in
      state.dirty <- false;
      state.processing <- true;
      Some key
  in
  Mutex.unlock queue.mutex;
  result

let task_done queue key =
  Mutex.lock queue.mutex;
  (match Hashtbl.find_opt queue.states key with
  | None -> ()
  | Some state ->
      state.processing <- false;
      if state.dirty && not queue.closed then (
        Queue.add key queue.queue;
        Condition.signal queue.ready)
      else Hashtbl.remove queue.states key);
  Mutex.unlock queue.mutex

let close queue =
  Mutex.lock queue.mutex;
  queue.closed <- true;
  Condition.broadcast queue.ready;
  Mutex.unlock queue.mutex

let length queue =
  Mutex.lock queue.mutex;
  let result = Queue.length queue.queue in
  Mutex.unlock queue.mutex;
  result

module Scheduler = struct
  type queue = t
  type item = { at : float; key : Core.Object_key.t }

  type t = {
    cancel : Cancel.t;
    queue : queue;
    mutex : Mutex.t;
    mutable items : item list;
    mutable stopped : bool;
    wake_read : Unix.file_descr;
    wake_write : Unix.file_descr;
    mutable thread : Thread.t option;
  }

  let rec insert item = function
    | [] -> [ item ]
    | head :: _ as items when item.at < head.at -> item :: items
    | head :: rest -> head :: insert item rest

  let wake scheduler =
    try ignore (Unix.write_substring scheduler.wake_write "x" 0 1)
    with Unix.Unix_error _ -> ()

  let drain scheduler =
    let buffer = Bytes.create 64 in
    try
      while Unix.read scheduler.wake_read buffer 0 (Bytes.length buffer) > 0 do
        ()
      done
    with Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> ()

  let take_due scheduler now =
    Mutex.lock scheduler.mutex;
    let rec split due = function
      | head :: rest when head.at <= now -> split (head :: due) rest
      | future -> (List.rev due, future)
    in
    let due, future = split [] scheduler.items in
    scheduler.items <- future;
    let stopped = scheduler.stopped in
    let timeout =
      match future with
      | [] -> 1.0
      | head :: _ -> min 1.0 (max 0.0 (head.at -. now))
    in
    Mutex.unlock scheduler.mutex;
    (stopped, timeout, due)

  let rec run scheduler =
    let stopped, timeout, due = take_due scheduler (Clock.now ()) in
    List.iter (fun item -> add scheduler.queue item.key) due;
    if (not stopped) && not (Cancel.is_cancelled scheduler.cancel) then (
      (try
         let readable, _, _ =
           Unix.select [ scheduler.wake_read ] [] [] timeout
         in
         if readable <> [] then drain scheduler
       with Unix.Unix_error (Unix.EINTR, _, _) -> ());
      run scheduler)

  let create ~cancel queue =
    let wake_read, wake_write = Wakeup.create () in
    Unix.set_nonblock wake_read;
    Unix.set_nonblock wake_write;
    let scheduler =
      {
        cancel;
        queue;
        mutex = Mutex.create ();
        items = [];
        stopped = false;
        wake_read;
        wake_write;
        thread = None;
      }
    in
    let thread = Thread.create run scheduler in
    scheduler.thread <- Some thread;
    scheduler

  let schedule scheduler ~after key =
    Mutex.lock scheduler.mutex;
    let scheduled = not scheduler.stopped in
    if scheduled then
      scheduler.items <-
        insert { at = Clock.deadline after; key } scheduler.items;
    Mutex.unlock scheduler.mutex;
    if scheduled then wake scheduler

  let stop scheduler =
    Mutex.lock scheduler.mutex;
    let thread =
      if scheduler.stopped then None
      else (
        scheduler.stopped <- true;
        let thread = scheduler.thread in
        scheduler.thread <- None;
        thread)
    in
    Mutex.unlock scheduler.mutex;
    match thread with
    | None -> ()
    | Some thread -> (
        wake scheduler;
        Thread.join thread;
        (try Unix.close scheduler.wake_read with Unix.Unix_error _ -> ());
        try Unix.close scheduler.wake_write with Unix.Unix_error _ -> ())
end
