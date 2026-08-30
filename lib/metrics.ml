type kind = Counter_kind | Gauge_kind | Histogram_kind of float array
type simple = { lock : Mutex.t; mutable value : float }

type histogram = {
  lock : Mutex.t;
  buckets : float array;
  counts : int array;
  mutable count : int;
  mutable sum : float;
}

type sample_data = Simple of simple | Histogram of histogram

type sample = {
  labels : (string * string) list;
  label_key : string;
  data : sample_data;
}

type family = {
  name : string;
  help : string;
  kind : kind;
  label_names : string list;
  samples : (string, sample) Hashtbl.t;
}

type t = { lock : Mutex.t; families : (string, family) Hashtbl.t }
type registry = t

let create () = { lock = Mutex.create (); families = Hashtbl.create 17 }
let content_type = "text/plain; version=0.0.4; charset=utf-8"

let valid_first = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' | ':' -> true
  | _ -> false

let valid_rest = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | ':' -> true
  | _ -> false

let valid_name value =
  value <> "" && valid_first value.[0] && String.for_all valid_rest value

let valid_label_first = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false

let valid_label_rest = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

let valid_label_name value =
  value <> ""
  && valid_label_first value.[0]
  && String.for_all valid_label_rest value

let normalize_labels labels =
  let labels = List.sort (fun (a, _) (b, _) -> String.compare a b) labels in
  let rec validate previous = function
    | [] -> ()
    | (name, _) :: rest ->
        if not (valid_label_name name) then
          invalid_arg ("Metrics: invalid label name " ^ name);
        if previous = Some name then
          invalid_arg ("Metrics: duplicate label name " ^ name);
        validate (Some name) rest
  in
  validate None labels;
  labels

let kind_name = function
  | Counter_kind -> "counter"
  | Gauge_kind -> "gauge"
  | Histogram_kind _ -> "histogram"

let kind_compatible a b =
  match (a, b) with
  | Counter_kind, Counter_kind | Gauge_kind, Gauge_kind -> true
  | Histogram_kind a, Histogram_kind b -> Array.to_list a = Array.to_list b
  | _ -> false

let label_key labels = labels |> List.map snd |> String.concat "\000"

let register registry ~name ~help ~kind ~labels =
  if not (valid_name name) then
    invalid_arg ("Metrics: invalid metric name " ^ name);
  if String.trim help = "" then invalid_arg "Metrics: help must not be empty";
  let labels = normalize_labels labels in
  let label_names = List.map fst labels in
  let key = label_key labels in
  Mutex.lock registry.lock;
  let result =
    Fun.protect
      ~finally:(fun () -> Mutex.unlock registry.lock)
      (fun () ->
        let family =
          match Hashtbl.find_opt registry.families name with
          | None ->
              let family =
                { name; help; kind; label_names; samples = Hashtbl.create 7 }
              in
              Hashtbl.add registry.families name family;
              family
          | Some family ->
              if family.help <> help then
                invalid_arg ("Metrics: conflicting help for " ^ name);
              if not (kind_compatible family.kind kind) then
                invalid_arg ("Metrics: conflicting type for " ^ name);
              if family.label_names <> label_names then
                invalid_arg ("Metrics: conflicting labels for " ^ name);
              family
        in
        match Hashtbl.find_opt family.samples key with
        | Some sample -> sample
        | None ->
            let data =
              match kind with
              | Counter_kind | Gauge_kind ->
                  Simple { lock = Mutex.create (); value = 0. }
              | Histogram_kind buckets ->
                  Histogram
                    {
                      lock = Mutex.create ();
                      buckets = Array.copy buckets;
                      counts = Array.make (Array.length buckets) 0;
                      count = 0;
                      sum = 0.;
                    }
            in
            let sample = { labels; label_key = key; data } in
            Hashtbl.add family.samples key sample;
            sample)
  in
  result

let finite value =
  if not (Float.is_finite value) then
    invalid_arg "Metrics: observations must be finite"

let simple_value sample =
  match sample.data with
  | Simple value -> value
  | Histogram _ -> invalid_arg "Metrics: internal metric type mismatch"

module Counter = struct
  type t = sample

  let create ~registry ~name ~help ?(labels = []) () =
    register registry ~name ~help ~kind:Counter_kind ~labels

  let add sample amount =
    finite amount;
    if amount < 0. then invalid_arg "Metrics.Counter.add: negative amount";
    let simple = simple_value sample in
    Mutex.lock simple.lock;
    simple.value <- simple.value +. amount;
    Mutex.unlock simple.lock

  let inc sample = add sample 1.

  let value sample =
    let simple = simple_value sample in
    Mutex.lock simple.lock;
    let value = simple.value in
    Mutex.unlock simple.lock;
    value
end

module Gauge = struct
  type t = sample

  let create ~registry ~name ~help ?(labels = []) () =
    register registry ~name ~help ~kind:Gauge_kind ~labels

  let set sample value =
    finite value;
    let simple = simple_value sample in
    Mutex.lock simple.lock;
    simple.value <- value;
    Mutex.unlock simple.lock

  let add sample amount =
    finite amount;
    let simple = simple_value sample in
    Mutex.lock simple.lock;
    simple.value <- simple.value +. amount;
    Mutex.unlock simple.lock

  let inc sample = add sample 1.
  let dec sample = add sample (-1.)
  let value = Counter.value
end

module Histogram = struct
  type t = sample

  let create ~registry ~name ~help ~buckets ?(labels = []) () =
    let rec validate previous = function
      | [] -> ()
      | value :: rest ->
          finite value;
          if
            Option.fold ~none:false
              ~some:(fun previous -> value <= previous)
              previous
          then
            invalid_arg
              "Metrics.Histogram.create: buckets must be strictly increasing";
          validate (Some value) rest
    in
    validate None buckets;
    if List.exists (fun (name, _) -> name = "le") labels then
      invalid_arg "Metrics.Histogram.create: label 'le' is reserved";
    register registry ~name ~help
      ~kind:(Histogram_kind (Array.of_list buckets))
      ~labels

  let observe sample value =
    finite value;
    match sample.data with
    | Simple _ -> invalid_arg "Metrics: internal metric type mismatch"
    | Histogram histogram ->
        Mutex.lock histogram.lock;
        histogram.count <- histogram.count + 1;
        histogram.sum <- histogram.sum +. value;
        Array.iteri
          (fun index boundary ->
            if value <= boundary then
              histogram.counts.(index) <- histogram.counts.(index) + 1)
          histogram.buckets;
        Mutex.unlock histogram.lock
end

let escape_help value =
  let output = Buffer.create (String.length value) in
  String.iter
    (function
      | '\\' -> Buffer.add_string output "\\\\"
      | '\n' -> Buffer.add_string output "\\n"
      | character -> Buffer.add_char output character)
    value;
  Buffer.contents output

let escape_label value =
  let output = Buffer.create (String.length value) in
  String.iter
    (function
      | '\\' -> Buffer.add_string output "\\\\"
      | '"' -> Buffer.add_string output "\\\""
      | '\n' -> Buffer.add_string output "\\n"
      | character -> Buffer.add_char output character)
    value;
  Buffer.contents output

let format_float value = Printf.sprintf "%.17g" value

let format_labels labels =
  match labels with
  | [] -> ""
  | labels ->
      let fields =
        List.map
          (fun (name, value) -> name ^ "=\"" ^ escape_label value ^ "\"")
          labels
      in
      "{" ^ String.concat "," fields ^ "}"

let render registry =
  Mutex.lock registry.lock;
  let families =
    Hashtbl.fold
      (fun _ family accumulator -> family :: accumulator)
      registry.families []
    |> List.sort (fun a b -> String.compare a.name b.name)
    |> List.map (fun family ->
        let samples =
          Hashtbl.fold
            (fun _ sample accumulator -> sample :: accumulator)
            family.samples []
          |> List.sort (fun a b -> String.compare a.label_key b.label_key)
        in
        (family, samples))
  in
  Mutex.unlock registry.lock;
  let output = Buffer.create 4096 in
  List.iter
    (fun (family, samples) ->
      Buffer.add_string output
        ("# HELP " ^ family.name ^ " " ^ escape_help family.help ^ "\n");
      Buffer.add_string output
        ("# TYPE " ^ family.name ^ " " ^ kind_name family.kind ^ "\n");
      List.iter
        (fun sample ->
          match sample.data with
          | Simple simple ->
              Mutex.lock simple.lock;
              let value = simple.value in
              Mutex.unlock simple.lock;
              Buffer.add_string output
                (family.name
                ^ format_labels sample.labels
                ^ " " ^ format_float value ^ "\n")
          | Histogram histogram ->
              Mutex.lock histogram.lock;
              let buckets = Array.copy histogram.buckets in
              let counts = Array.copy histogram.counts in
              let count = histogram.count in
              let sum = histogram.sum in
              Mutex.unlock histogram.lock;
              Array.iteri
                (fun index boundary ->
                  Buffer.add_string output
                    (family.name ^ "_bucket"
                    ^ format_labels
                        (sample.labels @ [ ("le", format_float boundary) ])
                    ^ " "
                    ^ string_of_int counts.(index)
                    ^ "\n"))
                buckets;
              Buffer.add_string output
                (family.name ^ "_bucket"
                ^ format_labels (sample.labels @ [ ("le", "+Inf") ])
                ^ " " ^ string_of_int count ^ "\n");
              Buffer.add_string output
                (family.name ^ "_sum"
                ^ format_labels sample.labels
                ^ " " ^ format_float sum ^ "\n");
              Buffer.add_string output
                (family.name ^ "_count"
                ^ format_labels sample.labels
                ^ " " ^ string_of_int count ^ "\n"))
        samples)
    families;
  Buffer.contents output
