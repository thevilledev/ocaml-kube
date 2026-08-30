module K = Kube
module Api = K.Client.For (Greeting)
module Controller = K.Controller.Make (Greeting)

let client_error context error =
  failwith (context ^ ": " ^ Format.asprintf "%a" K.Client.pp_error error)

let require context = function
  | Ok value -> value
  | Error value -> client_error context value

let metadata namespace name =
  {
    K.Core.name;
    namespace = Some namespace;
    uid = None;
    resource_version = None;
    generation = None;
    deletion_timestamp = None;
    finalizers = [];
    owner_references = [];
    labels = [ ("ocaml-k8s.dev/integration", "multi-namespace") ];
    annotations = [];
  }

let resource namespace name message =
  Greeting.make ~metadata:(metadata namespace name)
    ~spec:{ Greeting.Spec.message } ()

let wait_until context predicate =
  let deadline = Unix.gettimeofday () +. 30.0 in
  while (not (predicate ())) && Unix.gettimeofday () < deadline do
    Unix.sleepf 0.02
  done;
  if not (predicate ()) then failwith ("timed out waiting for " ^ context)

let () =
  let kubeconfig = ref None in
  let context = ref None in
  let namespace_a = ref "ocaml-k8s-multi-a" in
  let namespace_b = ref "ocaml-k8s-multi-b" in
  let set option value = option := Some value in
  Arg.parse
    [
      ( "--kubeconfig",
        Arg.String (set kubeconfig),
        "PATH Kubernetes kubeconfig path" );
      ("--context", Arg.String (set context), "NAME Kubeconfig context");
      ( "--namespace-a",
        Arg.Set_string namespace_a,
        "NAME First namespace selected by the controller" );
      ( "--namespace-b",
        Arg.Set_string namespace_b,
        "NAME Second namespace selected by the controller" );
    ]
    (fun value -> raise (Arg.Bad ("unexpected argument: " ^ value)))
    "multi-namespace-check [OPTIONS]";
  if !namespace_a = !namespace_b then failwith "selected namespaces must differ";
  let config =
    match !kubeconfig with
    | Some path -> K.Config.load_kubeconfig ?context:!context path
    | None -> K.Config.load_default ?context:!context ()
  in
  let config =
    match config with
    | Ok config -> config
    | Error message -> failwith ("configuration error: " ^ message)
  in
  let client = K.Client.create config in
  let suffix = string_of_int (Unix.getpid ()) in
  let name_a = "multi-a-" ^ suffix in
  let name_b = "multi-b-" ^ suffix in
  let cancel = K.Cancel.create () in
  let controller_thread = ref None in
  let controller_result = Atomic.make None in
  let state_lock = Mutex.create () in
  let observed = Hashtbl.create 5 in
  let deleted = Hashtbl.create 2 in
  let inspect predicate =
    Mutex.lock state_lock;
    Fun.protect ~finally:(fun () -> Mutex.unlock state_lock) predicate
  in
  let cleanup () =
    K.Cancel.cancel cancel;
    Option.iter Thread.join !controller_thread;
    ignore (Api.delete client ~namespace:!namespace_a name_a);
    ignore (Api.delete client ~namespace:!namespace_b name_b);
    K.Client.close client
  in
  Fun.protect ~finally:cleanup (fun () ->
      Api.create client ~namespace:!namespace_a
        (resource !namespace_a name_a "initial-a")
      |> require "create first Greeting"
      |> ignore;
      Api.create client ~namespace:!namespace_b
        (resource !namespace_b name_b "initial-b")
      |> require "create second Greeting"
      |> ignore;
      controller_thread :=
        Some
          (Thread.create
             (fun () ->
               let result =
                 Controller.run ~cancel
                   ~namespaces:[ !namespace_a; !namespace_b ] ~workers:2 client
                   ~reconcile:(fun _ request ->
                     Mutex.lock state_lock;
                     (match request.Controller.resource with
                     | Some greeting ->
                         Hashtbl.replace observed
                           (K.Core.Object_key.to_string request.key)
                           greeting.Greeting.spec.message
                     | None ->
                         Hashtbl.replace deleted
                           (K.Core.Object_key.to_string request.key)
                           ());
                     Mutex.unlock state_lock;
                     Ok K.Controller.Done)
               in
               Atomic.set controller_result (Some result))
             ());
      let key_a = !namespace_a ^ "/" ^ name_a in
      let key_b = !namespace_b ^ "/" ^ name_b in
      wait_until "both namespace snapshots" (fun () ->
          inspect (fun () ->
              Hashtbl.find_opt observed key_a = Some "initial-a"
              && Hashtbl.find_opt observed key_b = Some "initial-b"));
      Api.patch client ~namespace:!namespace_a name_a
        (K.Client.Merge_patch
           (`Assoc [ ("spec", `Assoc [ ("message", `String "updated-a") ]) ]))
      |> require "patch first Greeting"
      |> ignore;
      wait_until "watch update in the first namespace" (fun () ->
          inspect (fun () -> Hashtbl.find_opt observed key_a = Some "updated-a"));
      Api.delete client ~namespace:!namespace_b name_b
      |> require "delete second Greeting";
      wait_until "watch deletion in the second namespace" (fun () ->
          inspect (fun () -> Hashtbl.mem deleted key_b));
      K.Cancel.cancel cancel;
      Option.iter Thread.join !controller_thread;
      controller_thread := None;
      match Atomic.get controller_result with
      | Some (Ok ()) ->
          Printf.printf
            "multi-namespace controller passed: %s updated, %s deleted\n%!"
            key_a key_b
      | Some (Error error) -> client_error "multi-namespace controller" error
      | None -> failwith "multi-namespace controller produced no result")
