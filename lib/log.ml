type level = Debug | Info | Warn | Error

type value =
  | String of string
  | Int of int
  | Int64 of int64
  | Float of float
  | Bool of bool
  | Redacted

type field = string * value

type event = {
  timestamp : Ptime.t;
  level : level;
  message : string;
  fields : field list;
}

type sink = event -> unit

type core = {
  sink : sink option;
  now : unit -> Ptime.t;
  min_level : int Atomic.t;
  dropped : int Atomic.t;
  lock : Mutex.t;
}

type t = { core : core; fields : field list }

let level_rank = function
  | Debug -> 0
  | Info -> 1
  | Warn -> 2
  | Error -> 3

let level_of_rank = function
  | value when value <= 0 -> Debug
  | 1 -> Info
  | 2 -> Warn
  | _ -> Error

let level_to_string = function
  | Debug -> "debug"
  | Info -> "info"
  | Warn -> "warn"
  | Error -> "error"

let reserved = [ "timestamp"; "level"; "message" ]

let valid_field_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '.' | '-' | '/' -> true
  | _ -> false

let validate_value name = function
  | Float value when not (Float.is_finite value) ->
      invalid_arg ("Log: non-finite field " ^ name)
  | String _ | Int _ | Int64 _ | Float _ | Bool _ | Redacted -> ()

let normalize_fields fields =
  let fields = List.sort (fun (a, _) (b, _) -> String.compare a b) fields in
  let rec validate previous = function
    | [] -> ()
    | (name, value) :: rest ->
        if name = "" || not (String.for_all valid_field_char name) then
          invalid_arg ("Log: invalid field name " ^ name);
        if List.mem name reserved then
          invalid_arg ("Log: reserved field name " ^ name);
        if previous = Some name then invalid_arg ("Log: duplicate field " ^ name);
        validate_value name value;
        validate (Some name) rest
  in
  validate None fields;
  fields

let merge_fields inherited local =
  let local = normalize_fields local in
  let local_names = List.map fst local in
  List.filter (fun (name, _) -> not (List.mem name local_names)) inherited
  @ local
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)

let create ?(min_level = Info) ?(now = Ptime_clock.now) ~sink () =
  {
    core =
      {
        sink = Some sink;
        now;
        min_level = Atomic.make (level_rank min_level);
        dropped = Atomic.make 0;
        lock = Mutex.create ();
      };
    fields = [];
  }

let null =
  {
    core =
      {
        sink = None;
        now = Ptime_clock.now;
        min_level = Atomic.make (level_rank Error);
        dropped = Atomic.make 0;
        lock = Mutex.create ();
      };
    fields = [];
  }

let value_to_yojson = function
  | String value -> `String value
  | Int value -> `Int value
  | Int64 value -> `Intlit (Int64.to_string value)
  | Float value -> `Float value
  | Bool value -> `Bool value
  | Redacted -> `String "[REDACTED]"

let event_to_yojson event =
  `Assoc
    ([
       ( "timestamp",
         `String (Ptime.to_rfc3339 ~frac_s:6 ~tz_offset_s:0 event.timestamp) );
       ("level", `String (level_to_string event.level));
       ("message", `String event.message);
     ]
    @ List.map (fun (name, value) -> (name, value_to_yojson value)) event.fields
    )

let stderr ?min_level () =
  create ?min_level
    ~sink:(fun event ->
      Yojson.Safe.to_channel Stdlib.stderr (event_to_yojson event);
      output_char Stdlib.stderr '\n';
      flush Stdlib.stderr)
    ()

let with_fields logger fields =
  { logger with fields = merge_fields logger.fields fields }

let with_name logger name =
  if String.trim name = "" then invalid_arg "Log.with_name: empty name";
  with_fields logger [ ("logger", String name) ]

let min_level logger = level_of_rank (Atomic.get logger.core.min_level)

let set_min_level logger level =
  Atomic.set logger.core.min_level (level_rank level)

let enabled logger level =
  logger.core.sink <> None
  && level_rank level >= Atomic.get logger.core.min_level

let dropped_events logger = Atomic.get logger.core.dropped

let log logger level ?(fields = []) message =
  if enabled logger level then (
    let event =
      {
        timestamp = logger.core.now ();
        level;
        message;
        fields = merge_fields logger.fields fields;
      }
    in
    Mutex.lock logger.core.lock;
    Fun.protect
      ~finally:(fun () -> Mutex.unlock logger.core.lock)
      (fun () ->
        match logger.core.sink with
        | None -> ()
        | Some sink -> (
            try sink event with _ -> Atomic.incr logger.core.dropped)))

let debug logger ?fields message = log logger Debug ?fields message
let info logger ?fields message = log logger Info ?fields message
let warn logger ?fields message = log logger Warn ?fields message
let error logger ?fields message = log logger Error ?fields message
