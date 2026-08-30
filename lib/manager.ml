type component = {
  id : int;
  name : string;
  dependencies : component list;
  run : client:Client.t -> cancel:Cancel.t -> (unit, Client.error) result;
}

let next_component_id = Atomic.make 0

let component ?(dependencies = []) ~name run =
  if String.trim name = "" then invalid_arg "Manager.component: empty name";
  { id = Atomic.fetch_and_add next_component_id 1; name; dependencies; run }

let component_name component = component.name

type error = { component : string; cause : Client.error }

let pp_error formatter error =
  Format.fprintf formatter "component %s failed: %a" error.component
    Client.pp_error error.cause

type t = {
  client : Client.t;
  cancel : Cancel.t;
  components : (int, component) Hashtbl.t;
  mutable order : component list;
  mutable started : bool;
}

let create ?cancel client =
  {
    client;
    cancel = Option.value ~default:(Cancel.create ()) cancel;
    components = Hashtbl.create 17;
    order = [];
    started = false;
  }

let add manager component =
  if manager.started then invalid_arg "Manager.add: manager already started";
  let rec register component =
    if not (Hashtbl.mem manager.components component.id) then (
      List.iter register component.dependencies;
      Hashtbl.add manager.components component.id component;
      manager.order <- component :: manager.order)
  in
  register component

let run manager =
  if manager.started then invalid_arg "Manager.run: manager already started";
  manager.started <- true;
  let components = List.rev manager.order in
  let logger = Log.with_name (Client.logger manager.client) "manager" in
  Log.info logger
    ~fields:[ ("components", Log.Int (List.length components)) ]
    "Controller manager starting";
  let lock = Mutex.create () in
  let changed = Condition.create () in
  let remaining = ref (List.length components) in
  let first_error = ref None in
  let finished component result =
    let should_cancel = ref false in
    Mutex.lock lock;
    decr remaining;
    (match result with
    | Error cause when !first_error = None ->
        first_error := Some { component = component.name; cause };
        should_cancel := true
    | _ -> ());
    Condition.broadcast changed;
    Mutex.unlock lock;
    if !should_cancel then Cancel.cancel manager.cancel
  in
  let run_component component =
    let component_logger =
      Log.with_fields logger [ ("component", Log.String component.name) ]
    in
    Log.debug component_logger "Manager component starting";
    let result =
      try component.run ~client:manager.client ~cancel:manager.cancel
      with exn ->
        Error
          (Client.Transport
             ("uncaught component exception: " ^ Printexc.to_string exn))
    in
    (match result with
    | Ok () -> Log.debug component_logger "Manager component stopped"
    | Error error ->
        Log.error component_logger
          ~fields:
            [
              ("error", Log.String (Format.asprintf "%a" Client.pp_error error));
            ]
          "Manager component failed");
    finished component result
  in
  let threads =
    List.map (fun component -> Thread.create run_component component) components
  in
  Mutex.lock lock;
  while !remaining > 0 && !first_error = None do
    Condition.wait changed lock
  done;
  let failed = !first_error <> None in
  Mutex.unlock lock;
  if failed then Cancel.cancel manager.cancel;
  List.iter Thread.join threads;
  let result =
    match !first_error with
    | Some error -> Error error
    | None -> Ok ()
  in
  Log.info logger
    ~fields:
      [
        ( "result",
          Log.String
            (match result with
            | Ok () -> "stopped"
            | Error _ -> "failed") );
      ]
    "Controller manager stopped";
  result
