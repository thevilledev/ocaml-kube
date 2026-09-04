(** Compilation-checked building blocks for ordinary operator work. *)

module K = Kube
module K8s = Kube_api_v1_36
module Greeting_api = K.Client.For (Greeting)
module Greeting_controller = K.Controller.Make (Greeting)
module Config_maps = K.Reconcile.Make (K8s.Core_v1.ConfigMap)
module Owns_config_maps = Greeting_controller.Owns (K8s.Core_v1.ConfigMap)

let desired_config_map (greeting : Greeting.t) =
  let namespace = Option.value ~default:"default" greeting.metadata.namespace in
  let metadata =
    K8s.Meta_v1.ObjectMeta.make
      ~name:(greeting.metadata.name ^ "-message")
      ~namespace
      ~labels:[ ("app.kubernetes.io/managed-by", "greeting-operator") ]
      ()
  in
  K8s.Core_v1.ConfigMap.make ~api_version:"v1" ~kind:"ConfigMap" ~metadata
    ~data:[ ("message", greeting.spec.message) ]
    ()

let apply_owned_config_map client cancel (greeting : Greeting.t) =
  Config_maps.apply_owned ~cancel client ~field_manager:"greeting-operator"
    ~owner_api:Greeting.api ~owner:greeting.metadata
    (desired_config_map greeting)

let update_status_with_conflict_retry client cancel key make_status =
  K.Retry.on_conflict ~cancel (fun () ->
      match
        Greeting_api.get ~cancel client ?namespace:key.K.Core.namespace key.name
      with
      | Error _ as error -> error
      | Ok current ->
          let status = make_status current in
          Greeting_api.patch_status ~cancel client ?namespace:key.namespace
            key.name
            (Greeting.status_merge_patch status))

let controller_component ~metrics ~health reconcile =
  let owned_config_maps = Owns_config_maps.make () in
  Greeting_controller.component ~name:"greeting" ~metrics ~health
    ~watches:[ owned_config_maps ] ~reconcile ()
