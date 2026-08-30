type check = { id : int; name : string; run : unit -> (unit, string) result }

type t = {
  lock : Mutex.t;
  mutable next_id : int;
  mutable liveness : check list;
  mutable readiness : check list;
}

type failure = { check : string; message : string }

let create () =
  { lock = Mutex.create (); next_id = 0; liveness = []; readiness = [] }

let protect health fn =
  Mutex.lock health.lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock health.lock) fn

let add select replace health ~name run =
  if String.trim name = "" then
    invalid_arg "Health: check name must not be empty";
  let id =
    protect health (fun () ->
        let checks = select health in
        if List.exists (fun check -> check.name = name) checks then
          invalid_arg ("Health: duplicate check " ^ name);
        let id = health.next_id in
        health.next_id <- id + 1;
        replace health ({ id; name; run } :: checks);
        id)
  in
  let registered = Atomic.make true in
  fun () ->
    if Atomic.compare_and_set registered true false then
      protect health (fun () ->
          replace health
            (List.filter (fun check -> check.id <> id) (select health)))

let add_liveness health =
  add
    (fun health -> health.liveness)
    (fun health checks -> health.liveness <- checks)
    health

let add_readiness health =
  add
    (fun health -> health.readiness)
    (fun health checks -> health.readiness <- checks)
    health

let run_checks checks =
  let failures =
    List.filter_map
      (fun check ->
        try
          match check.run () with
          | Ok () -> None
          | Error message -> Some { check = check.name; message }
        with exn ->
          Some { check = check.name; message = Printexc.to_string exn })
      checks
  in
  match failures with
  | [] -> Ok ()
  | failures -> Error failures

let liveness health = protect health (fun () -> health.liveness) |> run_checks
let readiness health = protect health (fun () -> health.readiness) |> run_checks

let pp_failures formatter failures =
  let pp_one formatter failure =
    Format.fprintf formatter "%s: %s" failure.check failure.message
  in
  Format.pp_print_list
    ~pp_sep:(fun formatter () -> Format.pp_print_string formatter "; ")
    pp_one formatter failures
