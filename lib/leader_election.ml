type config = {
  namespace : string;
  name : string;
  identity : string;
  lease_duration : float;
  renew_deadline : float;
  retry_period : float;
  release_on_cancel : bool;
}

let default ~namespace ~name ~identity =
  {
    namespace;
    name;
    identity;
    lease_duration = 15.;
    renew_deadline = 10.;
    retry_period = 2.;
    release_on_cancel = true;
  }

type phase = Waiting | Leading | Stopped
type 'a outcome = Cancelled_before_leadership | Finished of 'a

type error =
  | Invalid_config of string
  | Client_error of Client.error
  | Leadership_lost
  | Callback_failed of string

let pp_error formatter = function
  | Invalid_config message ->
      Format.fprintf formatter "invalid leader-election config: %s" message
  | Client_error error ->
      Format.fprintf formatter "leader-election API failure: %a" Client.pp_error
        error
  | Leadership_lost -> Format.pp_print_string formatter "leadership lost"
  | Callback_failed message ->
      Format.fprintf formatter "leader callback failed: %s" message

let ( let* ) result fn =
  match result with
  | Ok value -> fn value
  | Error _ as error -> error

let member name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let string_opt = function
  | `String value -> Some value
  | _ -> None

let int_opt = function
  | `Int value -> Some value
  | `Intlit value -> int_of_string_opt value
  | _ -> None

module Lease = struct
  type t = {
    metadata : Core.object_meta;
    holder_identity : string option;
    lease_duration_seconds : int option;
    acquire_time : string option;
    renew_time : string option;
    lease_transitions : int option;
  }

  let api =
    {
      Core.group = "coordination.k8s.io";
      version = "v1";
      kind = "Lease";
      plural = "leases";
      scope = Core.Namespaced;
    }

  let metadata value = value.metadata

  let of_json json =
    let* metadata = Core.object_meta_of_json json in
    let spec = Option.value ~default:(`Assoc []) (member "spec" json) in
    Ok
      {
        metadata;
        holder_identity = Option.bind (member "holderIdentity" spec) string_opt;
        lease_duration_seconds =
          Option.bind (member "leaseDurationSeconds" spec) int_opt;
        acquire_time = Option.bind (member "acquireTime" spec) string_opt;
        renew_time = Option.bind (member "renewTime" spec) string_opt;
        lease_transitions = Option.bind (member "leaseTransitions" spec) int_opt;
      }

  let to_json value =
    let optional name fn = function
      | None -> []
      | Some value -> [ (name, fn value) ]
    in
    `Assoc
      [
        ("apiVersion", `String "coordination.k8s.io/v1");
        ("kind", `String "Lease");
        ("metadata", Core.object_meta_to_json value.metadata);
        ( "spec",
          `Assoc
            (optional "holderIdentity"
               (fun value -> `String value)
               value.holder_identity
            @ optional "leaseDurationSeconds"
                (fun value -> `Int value)
                value.lease_duration_seconds
            @ optional "acquireTime"
                (fun value -> `String value)
                value.acquire_time
            @ optional "renewTime" (fun value -> `String value) value.renew_time
            @ optional "leaseTransitions"
                (fun value -> `Int value)
                value.lease_transitions) );
      ]
end

module Lease_api = Client.For (Lease)

type observation = {
  mutable lease : Lease.t option;
  mutable signature :
    (string option * int option * string option * string option * int option)
    option;
  mutable observed_at : float;
}

type attempt = Acquired | Waiting

let now_timestamp () =
  Ptime.to_rfc3339 ~frac_s:6 ~tz_offset_s:0 (Ptime_clock.now ())

let empty_metadata config =
  {
    Core.name = config.name;
    namespace = Some config.namespace;
    uid = None;
    resource_version = None;
    generation = None;
    deletion_timestamp = None;
    finalizers = [];
    owner_references = [];
    labels = [];
    annotations = [];
  }

let signature lease =
  ( lease.Lease.holder_identity,
    lease.lease_duration_seconds,
    lease.acquire_time,
    lease.renew_time,
    lease.lease_transitions )

let observe observation now lease =
  let candidate = signature lease in
  if observation.signature <> Some candidate then (
    observation.signature <- Some candidate;
    observation.observed_at <- now);
  observation.lease <- Some lease

let observed_lease_is_valid observation now =
  match observation.lease with
  | None -> false
  | Some lease ->
      let duration =
        Option.value ~default:0 lease.Lease.lease_duration_seconds
      in
      observation.observed_at +. float_of_int duration > now

let is_conflict = Client.Error.is_conflict
let is_not_found = Client.Error.is_not_found
let is_already_exists = Client.Error.is_already_exists

let retryable error =
  Client.Error.is_transient error || Client.Error.is_conflict error

let fresh_lease config =
  let timestamp = now_timestamp () in
  {
    Lease.metadata = empty_metadata config;
    holder_identity = Some config.identity;
    lease_duration_seconds = Some (int_of_float (ceil config.lease_duration));
    acquire_time = Some timestamp;
    renew_time = Some timestamp;
    lease_transitions = Some 0;
  }

let renewed_lease config (current : Lease.t) ~same_holder =
  let timestamp = now_timestamp () in
  {
    current with
    Lease.holder_identity = Some config.identity;
    lease_duration_seconds = Some (int_of_float (ceil config.lease_duration));
    acquire_time =
      (if same_holder then current.acquire_time else Some timestamp);
    renew_time = Some timestamp;
    lease_transitions =
      Some
        (Option.value ~default:0 current.lease_transitions
        + if same_holder then 0 else 1);
  }

let replace config client cancel observation current ~same_holder =
  match
    Lease_api.replace ~cancel client ~namespace:config.namespace config.name
      (renewed_lease config current ~same_holder)
  with
  | Ok lease ->
      observe observation (Clock.now ()) lease;
      Ok Acquired
  | Error error when is_conflict error -> Ok Waiting
  | Error error -> Error error

let slow_acquire_or_renew config client cancel observation =
  let now = Clock.now () in
  match
    Lease_api.get ~cancel client ~namespace:config.namespace config.name
  with
  | Error error when is_not_found error -> (
      match
        Lease_api.create ~cancel client ~namespace:config.namespace
          (fresh_lease config)
      with
      | Ok lease ->
          observe observation (Clock.now ()) lease;
          Ok Acquired
      | Error error when is_already_exists error || is_conflict error ->
          Ok Waiting
      | Error error -> Error error)
  | Error error -> Error error
  | Ok lease ->
      observe observation now lease;
      let same_holder = lease.Lease.holder_identity = Some config.identity in
      let held_by_other =
        match lease.holder_identity with
        | Some holder -> holder <> "" && not same_holder
        | None -> false
      in
      if held_by_other && observed_lease_is_valid observation now then
        Ok Waiting
      else replace config client cancel observation lease ~same_holder

let acquire_or_renew config client cancel observation =
  let now = Clock.now () in
  match observation.lease with
  | Some lease
    when lease.Lease.holder_identity = Some config.identity
         && observed_lease_is_valid observation now -> (
      match
        replace config client cancel observation lease ~same_holder:true
      with
      | Ok Acquired as acquired -> acquired
      | Ok Waiting -> slow_acquire_or_renew config client cancel observation
      | Error _ -> slow_acquire_or_renew config client cancel observation)
  | _ -> slow_acquire_or_renew config client cancel observation

let with_attempt_timeout ~parent timeout fn =
  let cancel = Cancel.create () in
  let unlink = Cancel.on_cancel parent (fun () -> Cancel.cancel cancel) in
  let timer =
    Thread.create
      (fun () -> if Cancel.sleep cancel timeout then Cancel.cancel cancel)
      ()
  in
  Fun.protect
    ~finally:(fun () ->
      Cancel.cancel cancel;
      unlink ();
      Thread.join timer)
    (fun () -> fn cancel)

let validate config =
  let finite value = Float.is_finite value in
  if String.trim config.namespace = "" then Error "namespace must not be empty"
  else if String.trim config.name = "" then Error "name must not be empty"
  else if String.trim config.identity = "" then
    Error "identity must not be empty"
  else if (not (finite config.lease_duration)) || config.lease_duration < 1.
  then Error "lease_duration must be finite and at least one second"
  else if (not (finite config.renew_deadline)) || config.renew_deadline <= 0.
  then Error "renew_deadline must be finite and positive"
  else if (not (finite config.retry_period)) || config.retry_period <= 0. then
    Error "retry_period must be finite and positive"
  else if config.lease_duration <= config.renew_deadline then
    Error "lease_duration must be greater than renew_deadline"
  else if config.renew_deadline <= 1.2 *. config.retry_period then
    Error "renew_deadline must be greater than retry_period * 1.2"
  else Ok ()

let logger client config =
  Log.with_name (Client.logger client) "leader_election" |> fun logger ->
  Log.with_fields logger
    [
      ("namespace", Log.String config.namespace);
      ("lease", Log.String config.name);
      ("identity", Log.String config.identity);
    ]

let client_error_field error =
  ("error", Log.String (Format.asprintf "%a" Client.pp_error error))

let release config client logger observation =
  let deadline = Clock.deadline config.renew_deadline in
  let parent = Cancel.create () in
  let request_timeout = max 0.05 (config.renew_deadline /. 2.) in
  let rec attempt () =
    if Clock.remaining deadline = 0. then ()
    else
      match
        with_attempt_timeout ~parent request_timeout (fun cancel ->
            Lease_api.get ~cancel client ~namespace:config.namespace config.name)
      with
      | Error error when is_not_found error -> ()
      | Error error ->
          Log.warn logger
            ~fields:[ client_error_field error ]
            "Lease release read failed; retrying";
          ignore (Cancel.sleep parent (min config.retry_period 0.1));
          attempt ()
      | Ok lease when lease.Lease.holder_identity <> Some config.identity -> ()
      | Ok lease -> (
          let timestamp = now_timestamp () in
          let released =
            {
              lease with
              Lease.holder_identity = Some "";
              lease_duration_seconds = Some 1;
              acquire_time = Some timestamp;
              renew_time = Some timestamp;
            }
          in
          match
            with_attempt_timeout ~parent request_timeout (fun cancel ->
                Lease_api.replace ~cancel client ~namespace:config.namespace
                  config.name released)
          with
          | Ok _ ->
              observation.lease <- None;
              observation.signature <- None;
              Log.info logger "Leadership lease released"
          | Error error ->
              Log.warn logger
                ~fields:[ client_error_field error ]
                "Lease release update failed; retrying";
              ignore (Cancel.sleep parent (min config.retry_period 0.1));
              attempt ())
  in
  attempt ()

let run ?cancel ?(on_phase = fun _ -> ()) client config callback =
  match validate config with
  | Error message -> Error (Invalid_config message)
  | Ok () -> (
      let logger = logger client config in
      let outer_cancel = Option.value ~default:(Cancel.create ()) cancel in
      let request_timeout = max 0.05 (config.renew_deadline /. 2.) in
      let observation =
        { lease = None; signature = None; observed_at = Clock.now () }
      in
      let random = Random.State.make_self_init () in
      let notify (phase : phase) = try on_phase phase with _ -> () in
      notify Waiting;
      Log.info logger "Waiting for leadership";
      let rec acquire () =
        if Cancel.is_cancelled outer_cancel then Ok false
        else
          match
            with_attempt_timeout ~parent:outer_cancel request_timeout
              (fun cancel -> acquire_or_renew config client cancel observation)
          with
          | Ok Acquired -> Ok true
          | Ok Waiting -> retry_acquire ()
          | Error error when retryable error ->
              Log.warn logger
                ~fields:[ client_error_field error ]
                "Leadership acquisition failed; retrying";
              retry_acquire
                ?minimum_delay:(Client.Error.suggested_delay error)
                ()
          | Error error -> Error (Client_error error)
      and retry_acquire ?minimum_delay () =
        let jitter =
          config.retry_period *. (1. +. (0.2 *. Random.State.float random 1.))
        in
        let delay =
          match minimum_delay with
          | None -> jitter
          | Some minimum -> max minimum jitter
        in
        if Cancel.sleep outer_cancel delay then acquire () else Ok false
      in
      match acquire () with
      | Error (Client_error error) as result ->
          Log.error logger
            ~fields:[ client_error_field error ]
            "Leadership acquisition failed";
          notify Stopped;
          result
      | Error _ as result ->
          notify Stopped;
          result
      | Ok false ->
          Log.info logger "Leadership wait cancelled";
          notify Stopped;
          Ok Cancelled_before_leadership
      | Ok true when Cancel.is_cancelled outer_cancel ->
          if config.release_on_cancel then
            release config client logger observation;
          Log.info logger "Leadership cancelled before controller start";
          notify Stopped;
          Ok Cancelled_before_leadership
      | Ok true ->
          notify Leading;
          Log.info logger "Leadership acquired";
          let leadership_cancel = Cancel.create () in
          let renewal_cancel = Cancel.create () in
          let unlink_outer =
            Cancel.on_cancel outer_cancel (fun () ->
                Cancel.cancel leadership_cancel;
                Cancel.cancel renewal_cancel)
          in
          let callback_lock = Mutex.create () in
          let callback_result = ref None in
          let callback_thread =
            Thread.create
              (fun () ->
                let result =
                  try Ok (callback leadership_cancel)
                  with exn -> Error (Printexc.to_string exn)
                in
                Mutex.lock callback_lock;
                callback_result := Some result;
                Mutex.unlock callback_lock;
                Cancel.cancel renewal_cancel)
              ()
          in
          let get_callback_result () =
            Mutex.lock callback_lock;
            let result = !callback_result in
            Mutex.unlock callback_lock;
            result
          in
          let last_success = ref (Clock.now ()) in
          let rec renew () =
            if Cancel.is_cancelled outer_cancel then `Cancelled
            else
              match get_callback_result () with
              | Some _ -> `Finished
              | None -> (
                  let result =
                    with_attempt_timeout ~parent:renewal_cancel request_timeout
                      (fun cancel ->
                        acquire_or_renew config client cancel observation)
                  in
                  let fatal =
                    match result with
                    | Ok Acquired ->
                        last_success := Clock.now ();
                        None
                    | Ok Waiting -> None
                    | Error error when retryable error ->
                        Log.warn logger
                          ~fields:[ client_error_field error ]
                          "Leadership renewal failed; retrying";
                        None
                    | Error error -> Some error
                  in
                  let retry_delay =
                    match result with
                    | Error error when retryable error ->
                        Option.value ~default:config.retry_period
                          (Option.map (max config.retry_period)
                             (Client.Error.suggested_delay error))
                    | Ok _ | Error _ -> config.retry_period
                  in
                  match fatal with
                  | Some error -> `Fatal error
                  | None ->
                      if Clock.elapsed !last_success >= config.renew_deadline
                      then `Lost
                      else if Cancel.sleep renewal_cancel retry_delay then
                        renew ()
                      else if Cancel.is_cancelled outer_cancel then `Cancelled
                      else if get_callback_result () <> None then `Finished
                      else `Lost)
          in
          let renewal_result = renew () in
          Cancel.cancel leadership_cancel;
          Thread.join callback_thread;
          unlink_outer ();
          if
            config.release_on_cancel
            && (renewal_result = `Cancelled || renewal_result = `Finished)
          then release config client logger observation;
          notify Stopped;
          let result =
            match (renewal_result, get_callback_result ()) with
            | `Lost, _ -> Error Leadership_lost
            | `Fatal error, _ -> Error (Client_error error)
            | (`Cancelled | `Finished), Some (Ok value) -> Ok (Finished value)
            | (`Cancelled | `Finished), Some (Error message) ->
                Error (Callback_failed message)
            | (`Cancelled | `Finished), None ->
                Error (Callback_failed "callback ended without a result")
          in
          (match result with
          | Ok _ -> Log.info logger "Leadership stopped"
          | Error Leadership_lost ->
              Log.error logger "Leadership lease was lost"
          | Error (Client_error error) ->
              Log.error logger
                ~fields:[ client_error_field error ]
                "Leadership stopped after an API failure"
          | Error (Callback_failed message) ->
              Log.error logger
                ~fields:[ ("error", Log.String message) ]
                "Leadership callback failed"
          | Error (Invalid_config _) -> assert false);
          result)
