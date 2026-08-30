type resource = {
  name : string;
  singular_name : string;
  namespaced : bool;
  kind : string;
  verbs : string list;
  short_names : string list;
  categories : string list;
}

type resource_list = { group_version : string; resources : resource list }
type group_version = { group_version : string; version : string }

type group = {
  name : string;
  versions : group_version list;
  preferred_version : group_version option;
}

let ( let* ) result fn =
  match result with
  | Ok value -> fn value
  | Error _ as error -> error

let member name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let string = function
  | `String value -> Some value
  | _ -> None

let bool = function
  | `Bool value -> Some value
  | _ -> None

let strings = function
  | `List values -> List.filter_map string values
  | _ -> []

let decode response =
  try Ok (Yojson.Safe.from_string response.Http.body)
  with Yojson.Json_error message -> Error (Client.Decode message)

let server_version ?cancel client =
  let* response = Client.raw ?cancel client `GET "/version" in
  decode response

let core_versions ?cancel client =
  let* response = Client.raw ?cancel client `GET "/api" in
  let* json = decode response in
  match member "versions" json with
  | Some value -> Ok (strings value)
  | None -> Error (Client.Decode "core discovery response has no versions")

let group_version_of_json json =
  match
    ( Option.bind (member "groupVersion" json) string,
      Option.bind (member "version" json) string )
  with
  | Some group_version, Some version -> Some { group_version; version }
  | _ -> None

let group_of_json json =
  match Option.bind (member "name" json) string with
  | None -> None
  | Some name ->
      let versions =
        match member "versions" json with
        | Some (`List values) -> List.filter_map group_version_of_json values
        | _ -> []
      in
      let preferred_version =
        Option.bind (member "preferredVersion" json) group_version_of_json
      in
      Some { name; versions; preferred_version }

let groups ?cancel client =
  let* response = Client.raw ?cancel client `GET "/apis" in
  let* json = decode response in
  match member "groups" json with
  | Some (`List values) -> Ok (List.filter_map group_of_json values)
  | _ -> Error (Client.Decode "API group discovery response has no groups")

let resource_of_json json =
  match
    ( Option.bind (member "name" json) string,
      Option.bind (member "namespaced" json) bool,
      Option.bind (member "kind" json) string )
  with
  | Some name, Some namespaced, Some kind ->
      Some
        {
          name;
          singular_name =
            Option.bind (member "singularName" json) string
            |> Option.value ~default:"";
          namespaced;
          kind;
          verbs = Option.fold ~none:[] ~some:strings (member "verbs" json);
          short_names =
            Option.fold ~none:[] ~some:strings (member "shortNames" json);
          categories =
            Option.fold ~none:[] ~some:strings (member "categories" json);
        }
  | _ -> None

let resources ?cancel client ~group ~version =
  let path =
    if group = "" then "/api/" ^ Uri.pct_encode ~component:`Path version
    else
      "/apis/"
      ^ Uri.pct_encode ~component:`Path group
      ^ "/"
      ^ Uri.pct_encode ~component:`Path version
  in
  let* response = Client.raw ?cancel client `GET path in
  let* json = decode response in
  let* group_version =
    match Option.bind (member "groupVersion" json) string with
    | Some value -> Ok value
    | None ->
        Error (Client.Decode "resource discovery response has no groupVersion")
  in
  match member "resources" json with
  | Some (`List values) ->
      Ok { group_version; resources = List.filter_map resource_of_json values }
  | _ -> Error (Client.Decode "resource discovery response has no resources")

module Mapper = struct
  type subresource = { name : string; kind : string; verbs : string list }

  type mapping = {
    api : Core.api;
    singular_name : string;
    verbs : string list;
    short_names : string list;
    categories : string list;
    subresources : subresource list;
  }

  type 'a state =
    | Empty
    | Loading of int
    | Ready of 'a
    | Failed of int * float * Client.error

  type t = {
    client : Client.t;
    lock : Mutex.t;
    changed : Condition.t;
    mutable generation : int;
    mutable core : string list state;
    mutable groups : group list state;
    resources : (string, resource_list state) Hashtbl.t;
  }

  let create client =
    {
      client;
      lock = Mutex.create ();
      changed = Condition.create ();
      generation = 0;
      core = Empty;
      groups = Empty;
      resources = Hashtbl.create 31;
    }

  let client mapper = mapper.client

  let with_lock mapper fn =
    Mutex.lock mapper.lock;
    Fun.protect ~finally:(fun () -> Mutex.unlock mapper.lock) fn

  let invalidate mapper =
    with_lock mapper (fun () ->
        mapper.generation <- mapper.generation + 1;
        mapper.core <- Empty;
        mapper.groups <- Empty;
        Hashtbl.clear mapper.resources;
        Condition.broadcast mapper.changed)

  let cancelled = function
    | None -> false
    | Some cancel -> Cancel.is_cancelled cancel

  let cancellation_error () = Client.Transport "discovery request cancelled"

  let wake_on_cancel mapper = function
    | None -> Fun.id
    | Some cancel ->
        Cancel.on_cancel cancel (fun () ->
            with_lock mapper (fun () -> Condition.broadcast mapper.changed))

  let wait_while_loading mapper ?cancel get =
    let unregister = wake_on_cancel mapper cancel in
    Fun.protect ~finally:unregister (fun () ->
        Mutex.lock mapper.lock;
        while
          (match get () with
            | Loading _ -> true
            | Empty | Ready _ | Failed _ -> false)
          && not (cancelled cancel)
        do
          Condition.wait mapper.changed mapper.lock
        done;
        let was_cancelled = cancelled cancel in
        Mutex.unlock mapper.lock;
        not was_cancelled)

  let load_exception context exn =
    Client.Transport
      (Printf.sprintf "%s discovery failed: %s" context (Printexc.to_string exn))

  let rec ensure_root mapper ?cancel ~context ~get ~set load =
    Mutex.lock mapper.lock;
    let decision =
      if cancelled cancel then `Return (Error (cancellation_error ()))
      else
        match get () with
        | Ready value -> `Return (Ok value)
        | Failed (generation, retry_at, error)
          when generation = mapper.generation && Clock.now () < retry_at ->
            `Return (Error error)
        | Failed _ | Empty ->
            let generation = mapper.generation in
            set (Loading generation);
            `Load generation
        | Loading _ -> `Wait
    in
    Mutex.unlock mapper.lock;
    match decision with
    | `Return result -> result
    | `Wait ->
        if wait_while_loading mapper ?cancel get then
          ensure_root mapper ?cancel ~context ~get ~set load
        else Error (cancellation_error ())
    | `Load generation ->
        let result =
          try load () with exn -> Error (load_exception context exn)
        in
        with_lock mapper (fun () ->
            if mapper.generation = generation then
              match get () with
              | Loading candidate when candidate = generation ->
                  set
                    (match result with
                    | Ok value -> Ready value
                    | Error _ when cancelled cancel -> Empty
                    | Error error ->
                        Failed (generation, Clock.deadline 1.0, error));
                  Condition.broadcast mapper.changed
              | Empty | Loading _ | Ready _ | Failed _ -> ());
        result

  let ensure_core mapper ?cancel () =
    ensure_root mapper ?cancel ~context:"core API versions"
      ~get:(fun () -> mapper.core)
      ~set:(fun state -> mapper.core <- state)
      (fun () -> core_versions ?cancel mapper.client)

  let ensure_groups mapper ?cancel () =
    ensure_root mapper ?cancel ~context:"API groups"
      ~get:(fun () -> mapper.groups)
      ~set:(fun state -> mapper.groups <- state)
      (fun () -> groups ?cancel mapper.client)

  let group_version_key group version = group ^ "\000" ^ version

  let expected_group_version group version =
    if group = "" then version else group ^ "/" ^ version

  let rec ensure_resources mapper ?cancel ~group ~version () =
    let key = group_version_key group version in
    let get () =
      Option.value ~default:Empty (Hashtbl.find_opt mapper.resources key)
    in
    Mutex.lock mapper.lock;
    let decision =
      if cancelled cancel then `Return (Error (cancellation_error ()))
      else
        match get () with
        | Ready value -> `Return (Ok value)
        | Failed (generation, retry_at, error)
          when generation = mapper.generation && Clock.now () < retry_at ->
            `Return (Error error)
        | Failed _ | Empty ->
            let generation = mapper.generation in
            Hashtbl.replace mapper.resources key (Loading generation);
            `Load generation
        | Loading _ -> `Wait
    in
    Mutex.unlock mapper.lock;
    match decision with
    | `Return result -> result
    | `Wait ->
        if wait_while_loading mapper ?cancel get then
          ensure_resources mapper ?cancel ~group ~version ()
        else Error (cancellation_error ())
    | `Load generation ->
        let context = expected_group_version group version in
        let result =
          try
            let* discovered = resources ?cancel mapper.client ~group ~version in
            if discovered.group_version = context then Ok discovered
            else
              Error
                (Client.Decode
                   (Printf.sprintf
                      "resource discovery for %s returned groupVersion %s"
                      context discovered.group_version))
          with exn -> Error (load_exception context exn)
        in
        with_lock mapper (fun () ->
            if mapper.generation = generation then
              match Hashtbl.find_opt mapper.resources key with
              | Some (Loading candidate) when candidate = generation ->
                  Hashtbl.replace mapper.resources key
                    (match result with
                    | Ok value -> Ready value
                    | Error _ when cancelled cancel -> Empty
                    | Error error ->
                        Failed (generation, Clock.deadline 1.0, error));
                  Condition.broadcast mapper.changed
              | None | Some (Empty | Loading _ | Ready _ | Failed _) -> ());
        result

  let base_name name =
    match String.index_opt name '/' with
    | None -> Some name
    | Some _ -> None

  let subresources (resources : resource list) plural =
    let prefix = plural ^ "/" in
    let prefix_length = String.length prefix in
    resources
    |> List.filter_map (fun (resource : resource) ->
        if
          String.length resource.name > prefix_length
          && String.starts_with ~prefix resource.name
        then
          Some
            {
              name =
                String.sub resource.name prefix_length
                  (String.length resource.name - prefix_length);
              kind = resource.kind;
              verbs = resource.verbs;
            }
        else None)
    |> List.sort (fun left right -> String.compare left.name right.name)

  let mapping_of_resource ~group ~version resources (resource : resource) =
    {
      api =
        {
          Core.group;
          version;
          kind = resource.kind;
          plural = resource.name;
          scope =
            (if resource.namespaced then Core.Namespaced else Core.Cluster);
        };
      singular_name = resource.singular_name;
      verbs = resource.verbs;
      short_names = resource.short_names;
      categories = resource.categories;
      subresources = subresources resources resource.name;
    }

  let mappings_of_resource_list ~group ~version (discovered : resource_list) =
    discovered.resources
    |> List.filter_map (fun (resource : resource) ->
        Option.map
          (fun _ ->
            mapping_of_resource ~group ~version discovered.resources resource)
          (base_name resource.name))
    |> List.sort (fun left right ->
        String.compare left.api.plural right.api.plural)

  let no_match description =
    Error (Client.Invalid_request ("discovery has no " ^ description))

  let unique_match description = function
    | [ mapping ] -> Ok mapping
    | [] -> no_match description
    | _ ->
        Error
          (Client.Invalid_request
             ("discovery returned multiple resources for " ^ description))

  let nonempty label value =
    if String.trim value = "" then
      Error
        (Client.Invalid_request ("discovery " ^ label ^ " must not be empty"))
    else Ok ()

  let resolve_gvk ?cancel mapper ~group ~version ~kind =
    let* () = nonempty "version" version in
    let* () = nonempty "kind" kind in
    let* discovered = ensure_resources mapper ?cancel ~group ~version () in
    mappings_of_resource_list ~group ~version discovered
    |> List.filter (fun mapping -> mapping.api.kind = kind)
    |> unique_match
         (Printf.sprintf "resource for GVK %s/%s, Kind=%s"
            (if group = "" then "core" else group)
            version kind)

  let resolve_gvr ?cancel mapper ~group ~version ~resource =
    let* () = nonempty "version" version in
    let* () = nonempty "resource" resource in
    let* discovered = ensure_resources mapper ?cancel ~group ~version () in
    mappings_of_resource_list ~group ~version discovered
    |> List.filter (fun mapping -> mapping.api.plural = resource)
    |> unique_match
         (Printf.sprintf "resource for GVR %s/%s, Resource=%s"
            (if group = "" then "core" else group)
            version resource)

  let validate_named_version (group : group) version =
    let expected = expected_group_version group.name version.version in
    if version.group_version = expected then Ok version.version
    else
      Error
        (Client.Decode
           (Printf.sprintf "API group %s advertises invalid groupVersion %s"
              group.name version.group_version))

  let preferred_named_version (group : group) =
    match group.preferred_version with
    | Some preferred ->
        let* version = validate_named_version group preferred in
        if
          not
            (List.exists
               (fun candidate ->
                 candidate.version = preferred.version
                 && candidate.group_version = preferred.group_version)
               group.versions)
        then
          Error
            (Client.Decode
               (Printf.sprintf
                  "API group %s preferred version %s is absent from versions"
                  group.name preferred.version))
        else Ok version
    | None -> (
        match group.versions with
        | version :: _ -> validate_named_version group version
        | [] ->
            Error
              (Client.Decode
                 (Printf.sprintf "API group %s advertises no versions"
                    group.name)))

  let preferred_version ?cancel mapper ~group =
    if group = "" then
      let* versions = ensure_core mapper ?cancel () in
      match versions with
      | version :: _ -> Ok version
      | [] -> Error (Client.Decode "core API advertises no versions")
    else
      let* groups = ensure_groups mapper ?cancel () in
      groups |> List.filter (fun (candidate : group) -> candidate.name = group)
      |> function
      | [ discovered ] -> preferred_named_version discovered
      | [] -> no_match (Printf.sprintf "API group %s" group)
      | _ ->
          Error
            (Client.Decode
               (Printf.sprintf "API discovery repeats group %s" group))

  let preferred_group_versions ?cancel mapper group =
    match group with
    | Some group ->
        let* version = preferred_version ?cancel mapper ~group in
        Ok [ (group, version) ]
    | None ->
        let* core = ensure_core mapper ?cancel () in
        let* groups = ensure_groups mapper ?cancel () in
        let* core_version =
          match core with
          | version :: _ -> Ok version
          | [] -> Error (Client.Decode "core API advertises no versions")
        in
        let rec named_versions accumulator = function
          | [] -> Ok (List.rev accumulator)
          | (group : group) :: rest ->
              let* version = preferred_named_version group in
              named_versions ((group.name, version) :: accumulator) rest
        in
        let* named = named_versions [] groups in
        Ok (("", core_version) :: named |> List.sort_uniq compare)

  let resolve_preferred ?cancel mapper ?group ~description matches =
    let* versions = preferred_group_versions ?cancel mapper group in
    let rec collect accumulator = function
      | [] -> unique_match description (List.rev accumulator)
      | (group, version) :: rest ->
          let* discovered =
            ensure_resources mapper ?cancel ~group ~version ()
          in
          let matches =
            mappings_of_resource_list ~group ~version discovered
            |> List.filter matches
          in
          collect (List.rev_append matches accumulator) rest
    in
    collect [] versions

  let resolve_kind ?cancel ?group mapper ~kind =
    let* () = nonempty "kind" kind in
    resolve_preferred ?cancel mapper ?group
      ~description:
        (Printf.sprintf "preferred resource for Kind=%s%s" kind
           (match group with
           | None -> ""
           | Some group -> ", Group=" ^ if group = "" then "core" else group))
      (fun mapping -> mapping.api.kind = kind)

  let resolve_resource ?cancel ?group mapper ~resource =
    let* () = nonempty "resource" resource in
    resolve_preferred ?cancel mapper ?group
      ~description:
        (Printf.sprintf "preferred mapping for Resource=%s%s" resource
           (match group with
           | None -> ""
           | Some group -> ", Group=" ^ if group = "" then "core" else group))
      (fun mapping ->
        mapping.api.plural = resource
        || mapping.singular_name = resource
        || List.mem resource mapping.short_names)

  let compare_mapping left right =
    match String.compare left.api.group right.api.group with
    | 0 -> (
        match String.compare left.api.version right.api.version with
        | 0 -> String.compare left.api.plural right.api.plural
        | order -> order)
    | order -> order

  let mappings ?cancel mapper =
    let* core = ensure_core mapper ?cancel () in
    let* named = ensure_groups mapper ?cancel () in
    let group_versions = List.map (fun version -> ("", version)) core in
    let rec add_named accumulator = function
      | [] -> Ok accumulator
      | (group : group) :: rest ->
          let rec add_versions accumulator = function
            | [] -> Ok accumulator
            | version :: versions ->
                let* version = validate_named_version group version in
                add_versions ((group.name, version) :: accumulator) versions
          in
          let* accumulator = add_versions accumulator group.versions in
          add_named accumulator rest
    in
    let* group_versions = add_named group_versions named in
    let group_versions = List.sort_uniq compare group_versions in
    let rec discover accumulator = function
      | [] -> Ok (List.sort compare_mapping accumulator)
      | (group, version) :: rest ->
          let* discovered =
            ensure_resources mapper ?cancel ~group ~version ()
          in
          discover
            (List.rev_append
               (mappings_of_resource_list ~group ~version discovered)
               accumulator)
            rest
    in
    discover [] group_versions

  let refresh ?cancel mapper =
    invalidate mapper;
    mappings ?cancel mapper
end
