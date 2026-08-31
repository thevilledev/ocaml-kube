module K = Kube
module Greeting_api = K.Client.For (Greeting)
module Greeting_finalizer = K.Controller.Finalizer (Greeting)
module Greeting_controller = K.Controller.Make (Greeting)

let finalizer = "greetings.demo.ocaml-kube.dev/finalizer"
let error_string error = Format.asprintf "%a" K.Client.pp_error error

let record_event recorder greeting ~type_ ~reason ~action ~note =
  let regarding =
    K.Core.object_reference Greeting.api greeting.Greeting.metadata
  in
  match K.Events.record ~regarding ~type_ ~reason ~action ~note recorder with
  | Ok () -> ()
  | Error error ->
      Format.eprintf "Event publication failed for %s: %a@."
        greeting.metadata.name K.Client.pp_error error

let reconcile recorder client (request : Greeting_controller.request) =
  match request.resource with
  | None -> Ok K.Controller.Done
  | Some greeting ->
      Greeting_finalizer.run ~cancel:request.cancel client greeting finalizer
        (function
        | Greeting_finalizer.Cleanup greeting ->
            Printf.printf "finalized %s\n%!"
              (K.Core.Object_key.to_string request.key);
            record_event recorder greeting ~type_:K.Events.Normal
              ~reason:"Finalized" ~action:"Finalize"
              ~note:"Greeting cleanup completed";
            Ok K.Controller.Done
        | Greeting_finalizer.Apply greeting -> (
            let metadata = greeting.metadata in
            let generation = Option.value ~default:0 metadata.generation in
            match greeting.status with
            | Some status
              when status.observed_generation = generation
                   && status.reconciled_message = greeting.spec.message
                   && status.phase = Greeting.Phase.Ready ->
                Ok K.Controller.Done
            | _ -> (
                let patch =
                  Greeting.status_merge_patch
                    {
                      Greeting.Status.observed_generation = generation;
                      reconciled_message = greeting.spec.message;
                      phase = Greeting.Phase.Ready;
                    }
                in
                match
                  Greeting_api.patch_status ~cancel:request.cancel client
                    ?namespace:metadata.namespace metadata.name patch
                with
                | Error error -> Error (error_string error)
                | Ok _ ->
                    Printf.printf "reconciled %s generation %d: %s\n%!"
                      (K.Core.Object_key.to_string request.key)
                      generation greeting.spec.message;
                    record_event recorder greeting ~type_:K.Events.Normal
                      ~reason:"Reconciled" ~action:"UpdateStatus"
                      ~note:
                        (Printf.sprintf "Reconciled generation %d" generation);
                    Ok K.Controller.Done)))

let () =
  let kubeconfig = ref None in
  let context = ref None in
  let namespace = ref None in
  let workers = ref 2 in
  let leader_elect = ref false in
  let leader_election_name = ref "kube-greeting-operator" in
  let leader_election_namespace = ref None in
  let identity = ref None in
  let diagnostics_address = ref "127.0.0.1" in
  let diagnostics_port = ref 0 in
  let set option value = option := Some value in
  let arguments =
    [
      ( "--kubeconfig",
        Arg.String (set kubeconfig),
        "PATH Kubernetes kubeconfig path" );
      ("--context", Arg.String (set context), "NAME Kubeconfig context");
      ( "--namespace",
        Arg.String (set namespace),
        "NAME Namespace to watch (all by default)" );
      ("--workers", Arg.Set_int workers, "N Maximum concurrent reconciliations");
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
    "kube-greeting-operator [OPTIONS]";
  if !diagnostics_port < 0 || !diagnostics_port > 65535 then
    raise (Arg.Bad "--diagnostics-port must be between 0 and 65535");
  let loaded =
    match !kubeconfig with
    | Some path -> K.Config.load_kubeconfig ?context:!context path
    | None -> K.Config.load_default ?context:!context ()
  in
  match loaded with
  | Error message ->
      Printf.eprintf "configuration error: %s\n%!" message;
      exit 2
  | Ok config -> (
      let cancel = K.Cancel.create () in
      let handle_signal _ = K.Cancel.cancel cancel in
      Sys.set_signal Sys.sigint (Sys.Signal_handle handle_signal);
      Sys.set_signal Sys.sigterm (Sys.Signal_handle handle_signal);
      let client = K.Client.create ~logger:(K.Log.stderr ()) config in
      let result =
        Fun.protect
          ~finally:(fun () -> K.Client.close client)
          (fun () ->
            let process_identity =
              Option.value
                ~default:
                  (Printf.sprintf "%s-%d" (Unix.gethostname ()) (Unix.getpid ()))
                !identity
            in
            let recorder =
              match
                K.Events.create ~client
                  ~reporting_controller:"demo.ocaml-kube.dev/greeting-controller"
                  ~reporting_instance:process_identity ()
              with
              | Ok recorder -> recorder
              | Error message ->
                  raise (Invalid_argument ("Event recorder: " ^ message))
            in
            let manager = K.Manager.create ~cancel client in
            let health = K.Health.create () in
            let metrics = K.Metrics.create () in
            let controller =
              Greeting_controller.component ?namespace:!namespace
                ~workers:!workers ~metrics ~health
                ~reconcile:(reconcile recorder) ()
            in
            let run_controller_manager manager_cancel =
              let manager = K.Manager.create ~cancel:manager_cancel client in
              K.Manager.add manager controller;
              K.Manager.run manager
            in
            let leader_ready = Atomic.make (not !leader_elect) in
            let _leader_readiness =
              if !leader_elect then
                Some
                  (K.Health.add_readiness health ~name:"leader-election"
                     (fun () ->
                       if Atomic.get leader_ready then Ok ()
                       else Error "not the active leader"))
              else None
            in
            (if !diagnostics_port <> 0 then
               let diagnostics =
                 K.Diagnostics.create ~address:!diagnostics_address
                   ~port:!diagnostics_port ~health ~metrics ()
               in
               K.Manager.add manager (K.Diagnostics.component diagnostics));
            (if not !leader_elect then K.Manager.add manager controller
             else
               let identity = process_identity in
               let lease_namespace =
                 Option.value
                   ~default:(Option.value ~default:"default" config.namespace)
                   !leader_election_namespace
               in
               let election =
                 K.Leader_election.default ~namespace:lease_namespace
                   ~name:!leader_election_name ~identity
               in
               let leader =
                 K.Metrics.Gauge.create ~registry:metrics
                   ~name:"ocaml_kube_leader"
                   ~help:"Whether this replica is leader." ()
               in
               let transitions =
                 K.Metrics.Counter.create ~registry:metrics
                   ~name:"ocaml_kube_leader_transitions_total"
                   ~help:"Leadership transitions observed by this replica." ()
               in
               let leader_component =
                 K.Manager.component ~name:"leader-election"
                   (fun ~client ~cancel ->
                     let on_phase = function
                       | K.Leader_election.Waiting ->
                           Atomic.set leader_ready false;
                           K.Metrics.Gauge.set leader 0.;
                           Printf.printf "waiting for leadership as %s\n%!"
                             identity
                       | K.Leader_election.Leading ->
                           Atomic.set leader_ready true;
                           K.Metrics.Gauge.set leader 1.;
                           K.Metrics.Counter.inc transitions;
                           Printf.printf "acquired leadership as %s\n%!"
                             identity
                       | K.Leader_election.Stopped ->
                           Atomic.set leader_ready false;
                           K.Metrics.Gauge.set leader 0.;
                           Printf.printf "stopped leader election as %s\n%!"
                             identity
                     in
                     match
                       K.Leader_election.run ~cancel ~on_phase client election
                         run_controller_manager
                     with
                     | Ok K.Leader_election.Cancelled_before_leadership -> Ok ()
                     | Ok (K.Leader_election.Finished (Ok ())) -> Ok ()
                     | Ok (K.Leader_election.Finished (Error error)) ->
                         Error
                           (K.Client.Transport
                              (Format.asprintf "%a" K.Manager.pp_error error))
                     | Error error ->
                         Error
                           (K.Client.Transport
                              (Format.asprintf "%a" K.Leader_election.pp_error
                                 error)))
               in
               K.Manager.add manager leader_component);
            Result.map_error
              (Format.asprintf "%a" K.Manager.pp_error)
              (K.Manager.run manager))
      in
      match result with
      | Ok () -> ()
      | Error message ->
          Format.eprintf "controller failed: %s@." message;
          exit 1)
