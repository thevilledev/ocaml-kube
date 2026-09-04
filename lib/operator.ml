type options = {
  kubeconfig : string option;
  context : string option;
  namespace : string option;
  workers : int;
  leader_elect : bool;
  leader_election_name : string;
  leader_election_namespace : string option;
  identity : string option;
  diagnostics_address : string;
  diagnostics_port : int;
}

module Options = struct
  let make ?kubeconfig ?context ?namespace ?(workers = 2)
      ?(leader_elect = false) ?leader_election_namespace ?identity
      ?(diagnostics_address = "127.0.0.1") ?(diagnostics_port = 0)
      ~leader_election_name () =
    {
      kubeconfig;
      context;
      namespace;
      workers;
      leader_elect;
      leader_election_name;
      leader_election_namespace;
      identity;
      diagnostics_address;
      diagnostics_port;
    }

  let parse ~name ~leader_election_name () =
    let kubeconfig = ref None in
    let context = ref None in
    let namespace = ref None in
    let workers = ref 2 in
    let leader_elect = ref false in
    let leader_election_name = ref leader_election_name in
    let leader_election_namespace = ref None in
    let identity = ref None in
    let diagnostics_address = ref "127.0.0.1" in
    let diagnostics_port = ref 0 in
    let set target value = target := Some value in
    let arguments =
      [
        ("--kubeconfig", Arg.String (set kubeconfig), "PATH Kubeconfig path");
        ("--context", Arg.String (set context), "NAME Kubeconfig context");
        ( "--namespace",
          Arg.String (set namespace),
          "NAME Namespace to watch (all by default)" );
        ("--workers", Arg.Set_int workers, "N Concurrent reconciliations");
        ( "--leader-elect",
          Arg.Set leader_elect,
          "Enable coordination.k8s.io Lease leader election" );
        ( "--leader-election-name",
          Arg.Set_string leader_election_name,
          "NAME Leader-election Lease name" );
        ( "--leader-election-namespace",
          Arg.String (set leader_election_namespace),
          "NAME Leader-election Lease namespace" );
        ( "--identity",
          Arg.String (set identity),
          "ID Unique leader-election candidate identity" );
        ( "--diagnostics-address",
          Arg.Set_string diagnostics_address,
          "IP Diagnostics bind address" );
        ( "--diagnostics-port",
          Arg.Set_int diagnostics_port,
          "PORT Diagnostics port (0 disables the server)" );
      ]
    in
    Arg.parse arguments
      (fun value -> raise (Arg.Bad ("unexpected argument: " ^ value)))
      (name ^ " [OPTIONS]");
    {
      kubeconfig = !kubeconfig;
      context = !context;
      namespace = !namespace;
      workers = !workers;
      leader_elect = !leader_elect;
      leader_election_name = !leader_election_name;
      leader_election_namespace = !leader_election_namespace;
      identity = !identity;
      diagnostics_address = !diagnostics_address;
      diagnostics_port = !diagnostics_port;
    }
end

type context = {
  client : Client.t;
  cancel : Cancel.t;
  health : Health.t;
  metrics : Metrics.t;
  namespace : string option;
  workers : int;
  identity : string;
}

let validate (options : options) =
  if options.workers < 1 then Error "--workers must be positive"
  else if options.diagnostics_port < 0 || options.diagnostics_port > 65535 then
    Error "--diagnostics-port must be between 0 and 65535"
  else if String.trim options.leader_election_name = "" then
    Error "--leader-election-name must not be empty"
  else if String.trim options.diagnostics_address = "" then
    Error "--diagnostics-address must not be empty"
  else
    let named flag = function
      | Some value when String.trim value = "" ->
          Error (flag ^ " must not be empty")
      | _ -> Ok ()
    in
    match named "--namespace" options.namespace with
    | Error _ as error -> error
    | Ok () -> (
        match
          named "--leader-election-namespace"
            options.leader_election_namespace
        with
        | Error _ as error -> error
        | Ok () -> named "--identity" options.identity)

let process_identity (options : options) =
  Option.value options.identity
    ~default:(Printf.sprintf "%s-%d" (Unix.gethostname ()) (Unix.getpid ()))

let manager_error error = Format.asprintf "%a" Manager.pp_error error

let run_components base_context components cancel =
  let manager = Manager.create ~cancel base_context.client in
  try
    List.iter (Manager.add manager) components;
    Result.map_error manager_error (Manager.run manager)
  with exn ->
    Error ("component manager failed: " ^ Printexc.to_string exn)

let run_with_client ?cancel ?health ?metrics options client ~components =
  match validate options with
  | Error _ as error -> error
  | Ok () -> (
      let cancel = Option.value ~default:(Cancel.create ()) cancel in
      let health = Option.value ~default:(Health.create ()) health in
      let metrics = Option.value ~default:(Metrics.create ()) metrics in
      let identity = process_identity options in
      let base_context =
        {
          client;
          cancel;
          health;
          metrics;
          namespace = options.namespace;
          workers = options.workers;
          identity;
        }
      in
      let manager = Manager.create ~cancel client in
      (if options.diagnostics_port <> 0 then
         let diagnostics =
           Diagnostics.create ~address:options.diagnostics_address
             ~port:options.diagnostics_port ~health ~metrics ()
         in
         Manager.add manager (Diagnostics.component diagnostics));
      let built =
        try Ok (components base_context)
        with exn ->
          Error ("component construction failed: " ^ Printexc.to_string exn)
      in
      let setup =
        match built with
        | Error _ as error -> error
        | Ok components when not options.leader_elect ->
            List.iter (Manager.add manager) components;
            Ok ()
        | Ok components ->
            let leader_ready = Atomic.make false in
            let _readiness =
              Health.add_readiness health ~name:"leader-election" (fun () ->
                  if Atomic.get leader_ready then Ok ()
                  else Error "not the active leader")
            in
            let leader =
              Metrics.Gauge.create ~registry:metrics ~name:"ocaml_kube_leader"
                ~help:"Whether this replica is the active leader." ()
            in
            let transitions =
              Metrics.Counter.create ~registry:metrics
                ~name:"ocaml_kube_leader_transitions_total"
                ~help:"Leadership transitions observed by this replica." ()
            in
            let namespace =
              Option.value options.leader_election_namespace
                ~default:
                  (Option.value (Client.config client).Config.namespace
                     ~default:"default")
            in
            let election =
              Leader_election.default ~namespace
                ~name:options.leader_election_name ~identity
            in
            let component =
              Manager.component ~name:"leader-election" (fun ~client ~cancel ->
                  let on_phase = function
                    | Leader_election.Waiting | Leader_election.Stopped ->
                        Atomic.set leader_ready false;
                        Metrics.Gauge.set leader 0.
                    | Leader_election.Leading ->
                        Atomic.set leader_ready true;
                        Metrics.Gauge.set leader 1.;
                        Metrics.Counter.inc transitions
                  in
                  match
                    Leader_election.run ~cancel ~on_phase client election
                      (run_components base_context components)
                  with
                  | Ok Leader_election.Cancelled_before_leadership -> Ok ()
                  | Ok (Leader_election.Finished (Ok ())) -> Ok ()
                  | Ok (Leader_election.Finished (Error message)) ->
                      Error (Client.Transport message)
                  | Error error ->
                      Error
                        (Client.Transport
                           (Format.asprintf "%a" Leader_election.pp_error error)))
            in
            Manager.add manager component;
            Ok ()
      in
      match setup with
      | Error _ as error -> error
      | Ok () -> Result.map_error manager_error (Manager.run manager))

let with_signal_handlers cancel enabled fn =
  if not enabled then fn ()
  else
    let handler = Sys.Signal_handle (fun _ -> Cancel.cancel cancel) in
    let previous_int = Sys.signal Sys.sigint handler in
    let previous_term = Sys.signal Sys.sigterm handler in
    Fun.protect
      ~finally:(fun () ->
        ignore (Sys.signal Sys.sigint previous_int);
        ignore (Sys.signal Sys.sigterm previous_term))
      fn

let run ?cancel ?(logger = Log.stderr ()) ?(install_signal_handlers = true)
    options ~components =
  match validate options with
  | Error _ as error -> error
  | Ok () -> (
      let loaded =
        match options.kubeconfig with
        | Some path -> Config.load_kubeconfig ?context:options.context path
        | None -> Config.load_default ?context:options.context ()
      in
      match loaded with
      | Error message -> Error ("configuration error: " ^ message)
      | Ok config ->
          let cancel = Option.value ~default:(Cancel.create ()) cancel in
          let client = Client.create ~logger config in
          Fun.protect
            ~finally:(fun () -> Client.close client)
            (fun () ->
              with_signal_handlers cancel install_signal_handlers (fun () ->
                  run_with_client ~cancel options client ~components)))
