module Make (Resource : Core.Resource) = struct
  module Key_set = Set.Make (struct
    type t = Core.Object_key.t

    let compare = Core.Object_key.compare
  end)

  module String_set = Set.Make (String)

  type index = {
    values_of : Resource.t -> string list;
    buckets : (string, Key_set.t) Hashtbl.t;
    memberships : (Core.Object_key.t, String_set.t) Hashtbl.t;
  }

  type t = {
    mutex : Mutex.t;
    values : (Core.Object_key.t, Resource.t) Hashtbl.t;
    indexes : (string, index) Hashtbl.t;
  }

  let create () =
    {
      mutex = Mutex.create ();
      values = Hashtbl.create 127;
      indexes = Hashtbl.create 7;
    }

  let protect store fn =
    Mutex.lock store.mutex;
    Fun.protect ~finally:(fun () -> Mutex.unlock store.mutex) fn

  let get store key =
    protect store (fun () -> Hashtbl.find_opt store.values key)

  let distinct_values values_of resource =
    values_of resource
    |> List.fold_left
         (fun values value -> String_set.add value values)
         String_set.empty

  let add_membership buckets memberships key values =
    Hashtbl.replace memberships key values;
    String_set.iter
      (fun value ->
        let keys =
          Option.value ~default:Key_set.empty (Hashtbl.find_opt buckets value)
        in
        Hashtbl.replace buckets value (Key_set.add key keys))
      values

  let index_add_values index key values =
    add_membership index.buckets index.memberships key values

  let index_remove index key =
    Option.iter
      (fun values ->
        String_set.iter
          (fun value ->
            match Hashtbl.find_opt index.buckets value with
            | None -> ()
            | Some keys ->
                let keys = Key_set.remove key keys in
                if Key_set.is_empty keys then Hashtbl.remove index.buckets value
                else Hashtbl.replace index.buckets value keys)
          values)
      (Hashtbl.find_opt index.memberships key);
    Hashtbl.remove index.memberships key

  let build_index values_of resources =
    let buckets = Hashtbl.create 127 in
    let memberships = Hashtbl.create (max 16 (Hashtbl.length resources * 2)) in
    Hashtbl.iter
      (fun key resource ->
        add_membership buckets memberships key
          (distinct_values values_of resource))
      resources;
    (buckets, memberships)

  let replace_table target replacement =
    Hashtbl.clear target;
    Hashtbl.iter (Hashtbl.replace target) replacement

  let add_index store ~name values_of =
    protect store (fun () ->
        if String.trim name = "" then Error "index name must not be empty"
        else if Hashtbl.mem store.indexes name then
          Error ("index already exists: " ^ name)
        else
          let buckets, memberships = build_index values_of store.values in
          let index = { values_of; buckets; memberships } in
          Hashtbl.add store.indexes name index;
          Ok ())

  let by_index store ~name value =
    protect store (fun () ->
        match Hashtbl.find_opt store.indexes name with
        | None -> Error ("unknown index: " ^ name)
        | Some index ->
            let keys =
              Option.value ~default:Key_set.empty
                (Hashtbl.find_opt index.buckets value)
            in
            Ok
              (Key_set.elements keys
              |> List.filter_map (Hashtbl.find_opt store.values)))

  let upsert store value =
    protect store (fun () ->
        let key = Core.key_of_meta (Resource.metadata value) in
        let previous = Hashtbl.find_opt store.values key in
        let index_values =
          Hashtbl.fold
            (fun _ index values ->
              (index, distinct_values index.values_of value) :: values)
            store.indexes []
        in
        Hashtbl.iter (fun _ index -> index_remove index key) store.indexes;
        Hashtbl.replace store.values key value;
        List.iter
          (fun (index, values) -> index_add_values index key values)
          index_values;
        previous)

  let remove store key =
    protect store (fun () ->
        let previous = Hashtbl.find_opt store.values key in
        Hashtbl.iter (fun _ index -> index_remove index key) store.indexes;
        Hashtbl.remove store.values key;
        previous)

  let replace_with_previous store values =
    protect store (fun () ->
        let replacement = Hashtbl.create (max 16 (List.length values * 2)) in
        List.iter
          (fun value ->
            Hashtbl.replace replacement
              (Core.key_of_meta (Resource.metadata value))
              value)
          values;
        let replaced =
          List.map
            (fun value ->
              let key = Core.key_of_meta (Resource.metadata value) in
              (Hashtbl.find_opt store.values key, value))
            values
        in
        let removed =
          Hashtbl.fold
            (fun key value accumulator ->
              if Hashtbl.mem replacement key then accumulator
              else value :: accumulator)
            store.values []
        in
        let rebuilt_indexes =
          Hashtbl.fold
            (fun _ index rebuilt ->
              let buckets, memberships =
                build_index index.values_of replacement
              in
              (index, buckets, memberships) :: rebuilt)
            store.indexes []
        in
        replace_table store.values replacement;
        List.iter
          (fun (index, buckets, memberships) ->
            replace_table index.buckets buckets;
            replace_table index.memberships memberships)
          rebuilt_indexes;
        (replaced, removed))

  let replace_namespace_with_previous store ~namespace values =
    protect store (fun () ->
        if Resource.api.scope = Core.Cluster then
          Error "namespace-scoped replacement requires a namespaced resource"
        else if String.trim namespace = "" then
          Error "replacement namespace must not be empty"
        else
          let seen = Hashtbl.create (max 16 (List.length values * 2)) in
          let rec prepare accumulator = function
            | [] -> Ok (List.rev accumulator)
            | value :: rest ->
                let metadata = Resource.metadata value in
                if metadata.namespace <> Some namespace then
                  Error
                    (Printf.sprintf
                       "resource %s belongs to namespace %s, expected %s"
                       metadata.name
                       (Option.value ~default:"<none>" metadata.namespace)
                       namespace)
                else
                  let key = Core.key_of_meta metadata in
                  if Hashtbl.mem seen key then
                    Error
                      (Printf.sprintf "duplicate resource key %s"
                         (Core.Object_key.to_string key))
                  else (
                    Hashtbl.add seen key ();
                    let index_values =
                      Hashtbl.fold
                        (fun _ index values ->
                          (index, distinct_values index.values_of value)
                          :: values)
                        store.indexes []
                    in
                    prepare ((key, value, index_values) :: accumulator) rest)
          in
          match prepare [] values with
          | Error _ as error -> error
          | Ok prepared ->
              let old_keys =
                Hashtbl.fold
                  (fun key _ keys ->
                    if key.Core.Object_key.namespace = Some namespace then
                      key :: keys
                    else keys)
                  store.values []
              in
              let replaced =
                List.map
                  (fun (key, value, _) ->
                    (Hashtbl.find_opt store.values key, value))
                  prepared
              in
              let removed =
                List.filter_map
                  (fun key ->
                    if Hashtbl.mem seen key then None
                    else Hashtbl.find_opt store.values key)
                  old_keys
              in
              List.iter
                (fun key ->
                  Hashtbl.iter
                    (fun _ index -> index_remove index key)
                    store.indexes;
                  Hashtbl.remove store.values key)
                old_keys;
              List.iter
                (fun (key, value, index_values) ->
                  Hashtbl.replace store.values key value;
                  List.iter
                    (fun (index, values) -> index_add_values index key values)
                    index_values)
                prepared;
              Ok (replaced, removed))

  let replace store values = snd (replace_with_previous store values)

  let items store =
    protect store (fun () ->
        Hashtbl.fold (fun _ value acc -> value :: acc) store.values [])

  let length store = protect store (fun () -> Hashtbl.length store.values)
end
