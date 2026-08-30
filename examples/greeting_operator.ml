module K = Kube

module Greeting = struct
  type spec = { message : string }
  type status = { observed_generation : int; reconciled_message : string }

  type t = {
    api_version : string;
    kind : string;
    metadata : K.Core.object_meta;
    spec : spec;
    status : status option;
  }

  let api =
    {
      K.Core.group = "demo.kube-ocaml.dev";
      version = "v1alpha1";
      kind = "Greeting";
      plural = "greetings";
      scope = Namespaced;
    }

  let metadata value = value.metadata

  let member name = function
    | `Assoc fields -> List.assoc_opt name fields
    | _ -> None

  let string = function
    | `String value -> Some value
    | _ -> None

  let int = function
    | `Int value -> Some value
    | _ -> None

  let of_json json =
    let ( let* ) result fn =
      match result with
      | Ok value -> fn value
      | Error _ as error -> error
    in
    let required_string name json =
      match Option.bind (member name json) string with
      | Some value -> Ok value
      | None -> Error (name ^ " is required")
    in
    let* api_version = required_string "apiVersion" json in
    let* kind = required_string "kind" json in
    let* metadata = K.Core.object_meta_of_json json in
    let* spec =
      match member "spec" json with
      | Some spec ->
          let* message = required_string "message" spec in
          Ok { message }
      | None -> Error "spec is required"
    in
    let status =
      match member "status" json with
      | Some status -> (
          match
            ( Option.bind (member "observedGeneration" status) int,
              Option.bind (member "reconciledMessage" status) string )
          with
          | Some observed_generation, Some reconciled_message ->
              Some { observed_generation; reconciled_message }
          | _ -> None)
      | None -> None
    in
    Ok { api_version; kind; metadata; spec; status }

  let to_json value =
    `Assoc
      ([
         ("apiVersion", `String value.api_version);
         ("kind", `String value.kind);
         ("metadata", K.Core.object_meta_to_json value.metadata);
         ("spec", `Assoc [ ("message", `String value.spec.message) ]);
       ]
      @
      match value.status with
      | None -> []
      | Some status ->
          [
            ( "status",
              `Assoc
                [
                  ("observedGeneration", `Int status.observed_generation);
                  ("reconciledMessage", `String status.reconciled_message);
                ] );
          ])
end

module Greeting_api = K.Client.For (Greeting)
module Greeting_finalizer = K.Controller.Finalizer (Greeting)
module Greeting_controller = K.Controller.Make (Greeting)

let finalizer = "greetings.demo.kube-ocaml.dev/finalizer"
let error_string error = Format.asprintf "%a" K.Client.pp_error error

let reconcile client (request : Greeting_controller.request) =
  match request.resource with
  | None -> Ok K.Controller.Done
  | Some greeting -> (
      let metadata = greeting.metadata in
      if metadata.deletion_timestamp <> None then
        match Greeting_finalizer.remove client greeting finalizer with
        | Ok _ ->
            Printf.printf "finalized %s\n%!"
              (K.Core.Object_key.to_string request.key);
            Ok K.Controller.Done
        | Error error -> Error (error_string error)
      else if not (List.mem finalizer metadata.finalizers) then
        match Greeting_finalizer.ensure client greeting finalizer with
        | Ok _ -> Ok K.Controller.Requeue
        | Error error -> Error (error_string error)
      else
        let generation = Option.value ~default:0 metadata.generation in
        match greeting.status with
        | Some status
          when status.observed_generation = generation
               && status.reconciled_message = greeting.spec.message ->
            Ok K.Controller.Done
        | _ -> (
            let patch =
              `Assoc
                [
                  ( "status",
                    `Assoc
                      [
                        ("observedGeneration", `Int generation);
                        ("reconciledMessage", `String greeting.spec.message);
                      ] );
                ]
            in
            match
              Greeting_api.patch_status client ?namespace:metadata.namespace
                metadata.name (K.Client.Merge_patch patch)
            with
            | Error error -> Error (error_string error)
            | Ok _ ->
                Printf.printf "reconciled %s generation %d: %s\n%!"
                  (K.Core.Object_key.to_string request.key)
                  generation greeting.spec.message;
                Ok K.Controller.Done))

let () =
  let kubeconfig = ref None in
  let context = ref None in
  let namespace = ref None in
  let workers = ref 2 in
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
    ]
  in
  Arg.parse arguments
    (fun value -> raise (Arg.Bad ("unexpected argument: " ^ value)))
    "kube-greeting-operator [OPTIONS]";
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
      let client = K.Client.create config in
      match
        Greeting_controller.run ~cancel ?namespace:!namespace ~workers:!workers
          client ~reconcile
      with
      | Ok () -> ()
      | Error error ->
          Format.eprintf "controller failed: %a@." K.Client.pp_error error;
          exit 1)
