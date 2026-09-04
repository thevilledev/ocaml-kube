(** Batteries-included process lifecycle for Kubernetes operators. *)

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

module Options : sig
  val make :
    ?kubeconfig:string ->
    ?context:string ->
    ?namespace:string ->
    ?workers:int ->
    ?leader_elect:bool ->
    ?leader_election_namespace:string ->
    ?identity:string ->
    ?diagnostics_address:string ->
    ?diagnostics_port:int ->
    leader_election_name:string ->
    unit ->
    options

  val parse : name:string -> leader_election_name:string -> unit -> options
  (** Parse the standard operator flags: kubeconfig/context, watch namespace,
      worker count, Lease leadership, process identity, and diagnostics bind.
      Diagnostics default to disabled for local development; pass
      [--diagnostics-port 8080] in a Deployment. *)
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
(** Shared dependencies provided while building controller components.
    [cancel] is the process lifetime. A component's run callback receives the
    narrower manager token, including leadership loss when election is enabled.
*)

val run_with_client :
  ?cancel:Cancel.t ->
  ?health:Health.t ->
  ?metrics:Metrics.t ->
  options ->
  Client.t ->
  components:(context -> Manager.component list) ->
  (unit, string) result
(** Supervise components, optional diagnostics, and optional Lease leadership
    around an existing client. The client remains owned by the caller. *)

val run :
  ?cancel:Cancel.t ->
  ?logger:Log.t ->
  ?install_signal_handlers:bool ->
  options ->
  components:(context -> Manager.component list) ->
  (unit, string) result
(** Load configuration, create and close the client, and supervise the whole
    operator. SIGINT and SIGTERM trigger graceful cancellation by default; prior
    handlers are restored before returning. *)
