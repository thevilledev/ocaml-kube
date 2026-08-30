module Make (Resource : Core.Resource) = struct
  type t = {
    mutex : Mutex.t;
    values : (Core.Object_key.t, Resource.t) Hashtbl.t;
  }

  let create () = { mutex = Mutex.create (); values = Hashtbl.create 127 }

  let protect store fn =
    Mutex.lock store.mutex;
    Fun.protect ~finally:(fun () -> Mutex.unlock store.mutex) fn

  let get store key =
    protect store (fun () -> Hashtbl.find_opt store.values key)

  let upsert store value =
    protect store (fun () ->
        let key = Core.key_of_meta (Resource.metadata value) in
        Hashtbl.replace store.values key value)

  let remove store key =
    protect store (fun () -> Hashtbl.remove store.values key)

  let replace store values =
    protect store (fun () ->
        let replacement = Hashtbl.create (max 16 (List.length values * 2)) in
        List.iter
          (fun value ->
            Hashtbl.replace replacement
              (Core.key_of_meta (Resource.metadata value))
              value)
          values;
        let removed =
          Hashtbl.fold
            (fun key _ accumulator ->
              if Hashtbl.mem replacement key then accumulator
              else key :: accumulator)
            store.values []
        in
        Hashtbl.clear store.values;
        Hashtbl.iter (Hashtbl.replace store.values) replacement;
        removed)

  let items store =
    protect store (fun () ->
        Hashtbl.fold (fun _ value acc -> value :: acc) store.values [])

  let length store = protect store (fun () -> Hashtbl.length store.values)
end
