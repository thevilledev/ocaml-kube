module K = Kube
module Greeting_api = K.Client.For (Greeting)
module Greeting_finalizer = K.Controller.Finalizer (Greeting)
module Greeting_controller = K.Controller.Make (Greeting)

let finalizer = "greetings.demo.ocaml-kube.dev/finalizer"
let error_string error = Format.asprintf "%a" K.Client.pp_error error

type instrumentation = {
  updated : K.Metrics.Counter.t;
  unchanged : K.Metrics.Counter.t;
  finalized : K.Metrics.Counter.t;
  message_bytes : K.Metrics.Histogram.t;
}

let instrumentation registry =
  let outcome value =
    K.Metrics.Counter.create ~registry ~name:"greeting_reconciles_total"
      ~help:"Greeting reconciliation outcomes."
      ~labels:[ ("outcome", value) ]
      ()
  in
  {
    updated = outcome "updated";
    unchanged = outcome "unchanged";
    finalized = outcome "finalized";
    message_bytes =
      K.Metrics.Histogram.create ~registry ~name:"greeting_message_bytes"
        ~help:"Size of reconciled Greeting messages."
        ~buckets:[ 16.; 64.; 256.; 1024. ] ();
  }

let record_event ~cancel logger recorder greeting ~type_ ~reason ~action ~note =
  let regarding =
    K.Core.object_reference Greeting.api greeting.Greeting.metadata
  in
  match
    K.Events.record ~cancel ~regarding ~type_ ~reason ~action ~note recorder
  with
  | Ok () -> ()
  | Error error ->
      K.Log.warn logger
        ~fields:
          [
            ("resource", K.Log.String greeting.metadata.name);
            ("error", K.Log.String (error_string error));
          ]
        "Event publication failed"

let reconcile telemetry recorder client (request : Greeting_controller.request)
    =
  match request.resource with
  | None -> Ok K.Controller.Done
  | Some greeting ->
      let logger =
        K.Client.logger client |> fun logger ->
        K.Log.with_name logger "greeting" |> fun logger ->
        K.Log.with_fields logger
          [ ("key", K.Log.String (K.Core.Object_key.to_string request.key)) ]
      in
      Greeting_finalizer.run ~cancel:request.cancel client greeting finalizer
        (function
        | Greeting_finalizer.Cleanup greeting ->
            K.Metrics.Counter.inc telemetry.finalized;
            K.Log.info logger "Greeting cleanup completed";
            record_event ~cancel:request.cancel logger recorder greeting
              ~type_:K.Events.Normal ~reason:"Finalized" ~action:"Finalize"
              ~note:"Greeting cleanup completed";
            Ok K.Controller.Done
        | Greeting_finalizer.Apply greeting -> (
            let metadata = greeting.metadata in
            let generation = Option.value ~default:0 metadata.generation in
            match greeting.status with
            | Some status
              when status.observed_generation = generation
                   && status.reconciled_message = greeting.spec.message
                   && status.phase = Greeting.Phase.Ready
                   && Kube_crd.Condition.is_true "Ready" status.conditions ->
                K.Metrics.Counter.inc telemetry.unchanged;
                K.Log.debug logger "Greeting is already converged";
                Ok K.Controller.Done
            | _ -> (
                let previous_conditions =
                  match greeting.status with
                  | None -> []
                  | Some status -> status.conditions
                in
                let ready =
                  Kube_crd.Condition.make ~type_:"Ready"
                    ~status:Kube_crd.Condition.True
                    ~observed_generation:(Int64.of_int generation)
                    ~reason:"Reconciled"
                    ~message:"The desired message has been reconciled" ()
                in
                let conditions, _ =
                  Kube_crd.Condition.set ready previous_conditions
                in
                let patch =
                  Greeting.status_merge_patch
                    {
                      Greeting.Status.observed_generation = generation;
                      reconciled_message = greeting.spec.message;
                      phase = Greeting.Phase.Ready;
                      conditions;
                    }
                in
                match
                  Greeting_api.patch_status ~cancel:request.cancel client
                    ?namespace:metadata.namespace metadata.name patch
                with
                | Error error -> Error (error_string error)
                | Ok _ ->
                    K.Metrics.Counter.inc telemetry.updated;
                    K.Metrics.Histogram.observe telemetry.message_bytes
                      (float_of_int (String.length greeting.spec.message));
                    K.Log.info logger
                      ~fields:[ ("generation", K.Log.Int generation) ]
                      "Greeting status updated";
                    record_event ~cancel:request.cancel logger recorder greeting
                      ~type_:K.Events.Normal ~reason:"Reconciled"
                      ~action:"UpdateStatus"
                      ~note:
                        (Printf.sprintf "Reconciled generation %d" generation);
                    Ok K.Controller.Done)))

let () =
  let options =
    K.Operator.Options.parse ~name:"kube-greeting-operator"
      ~leader_election_name:"kube-greeting-operator" ()
  in
  match
    K.Operator.run options ~components:(fun context ->
        let recorder =
          K.Events.create ~client:context.client
            ~reporting_controller:"demo.ocaml-kube.dev/greeting-controller"
            ~reporting_instance:context.identity ()
          |> Result.get_ok
        in
        let telemetry = instrumentation context.metrics in
        [
          Greeting_controller.component ?namespace:context.namespace
            ~workers:context.workers ~metrics:context.metrics
            ~health:context.health
            ~reconcile:(reconcile telemetry recorder)
            ();
        ])
  with
  | Ok () -> ()
  | Error message ->
      Format.eprintf "operator failed: %s@." message;
      exit 1
