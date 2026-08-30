type line = { number : int; indent : int; text : string }

exception Parse_error of int * string

let trim_right value =
  let rec last index =
    if index < 0 then -1
    else
      match value.[index] with
      | ' ' | '\t' | '\r' -> last (index - 1)
      | _ -> index
  in
  let index = last (String.length value - 1) in
  if index < 0 then "" else String.sub value 0 (index + 1)

let strip_comment value =
  let rec loop index quote escaped =
    if index = String.length value then value
    else
      let character = value.[index] in
      match quote with
      | Some '"' when escaped -> loop (index + 1) quote false
      | Some '"' when character = '\\' -> loop (index + 1) quote true
      | Some expected when character = expected -> loop (index + 1) None false
      | Some _ -> loop (index + 1) quote false
      | None when character = '\'' || character = '"' ->
          loop (index + 1) (Some character) false
      | None
        when character = '#'
             && (index = 0
                || value.[index - 1] = ' '
                || value.[index - 1] = '\t') -> String.sub value 0 index
      | None -> loop (index + 1) None false
  in
  loop 0 None false

let preprocess input =
  input |> String.split_on_char '\n'
  |> List.mapi (fun index raw -> (index + 1, trim_right (strip_comment raw)))
  |> List.filter_map (fun (number, raw) ->
      let length = String.length raw in
      let rec indentation index =
        if index < length && raw.[index] = ' ' then indentation (index + 1)
        else index
      in
      let indent = indentation 0 in
      if indent = length then None
      else
        let text = String.sub raw indent (length - indent) in
        if String.length text > 0 && text.[0] = '#' then None
        else if indent mod 2 <> 0 then
          raise
            (Parse_error (number, "indentation must use multiples of two spaces"))
        else Some { number; indent; text })
  |> Array.of_list

let unquote value =
  let length = String.length value in
  if length >= 2 && value.[0] = '"' && value.[length - 1] = '"' then
    try
      match Yojson.Safe.from_string value with
      | `String value -> value
      | _ -> value
    with _ -> String.sub value 1 (length - 2)
  else if length >= 2 && value.[0] = '\'' && value.[length - 1] = '\'' then
    String.sub value 1 (length - 2)
  else value

let split_flow separator value =
  let pieces = ref [] in
  let start = ref 0 in
  let depth = ref 0 in
  let quote = ref None in
  let escaped = ref false in
  String.iteri
    (fun index character ->
      match !quote with
      | Some '"' when !escaped -> escaped := false
      | Some '"' when character = '\\' -> escaped := true
      | Some expected when character = expected -> quote := None
      | Some _ -> ()
      | None when character = '\'' || character = '"' -> quote := Some character
      | None when character = '[' || character = '{' -> incr depth
      | None when character = ']' || character = '}' -> decr depth
      | None when character = separator && !depth = 0 ->
          pieces := String.sub value !start (index - !start) :: !pieces;
          start := index + 1
      | None -> ())
    value;
  pieces := String.sub value !start (String.length value - !start) :: !pieces;
  List.rev !pieces

let split_flow_pair value =
  let depth = ref 0 in
  let quote = ref None in
  let escaped = ref false in
  let found = ref None in
  String.iteri
    (fun index character ->
      if !found = None then
        match !quote with
        | Some '"' when !escaped -> escaped := false
        | Some '"' when character = '\\' -> escaped := true
        | Some expected when character = expected -> quote := None
        | Some _ -> ()
        | None when character = '\'' || character = '"' ->
            quote := Some character
        | None when character = '[' || character = '{' -> incr depth
        | None when character = ']' || character = '}' -> decr depth
        | None when character = ':' && !depth = 0 -> found := Some index
        | None -> ())
    value;
  match !found with
  | None -> None
  | Some index ->
      Some
        ( String.sub value 0 index,
          String.sub value (index + 1) (String.length value - index - 1) )

let rec scalar value =
  let value = String.trim value in
  let length = String.length value in
  if length >= 2 && value.[0] = '[' && value.[length - 1] = ']' then
    let inside = String.sub value 1 (length - 2) |> String.trim in
    if inside = "" then `List []
    else `List (List.map scalar (split_flow ',' inside))
  else if length >= 2 && value.[0] = '{' && value.[length - 1] = '}' then
    let inside = String.sub value 1 (length - 2) |> String.trim in
    if inside = "" then `Assoc []
    else
      `Assoc
        (split_flow ',' inside
        |> List.map (fun entry ->
            match split_flow_pair entry with
            | Some (key, value) -> (String.trim key |> unquote, scalar value)
            | None ->
                raise
                  (Parse_error
                     (0, "flow mapping entry must contain a top-level ':'"))))
  else
    match String.lowercase_ascii value with
    | "" | "null" | "~" -> `Null
    | "true" -> `Bool true
    | "false" -> `Bool false
    | _ -> (
        match int_of_string_opt value with
        | Some value -> `Int value
        | None -> (
            match float_of_string_opt value with
            | Some number when Float.is_finite number -> `Float number
            | _ -> `String (unquote value)))

let split_pair line text =
  match String.index_opt text ':' with
  | None ->
      raise (Parse_error (line, "expected a mapping entry containing ':'"))
  | Some index ->
      let key = String.sub text 0 index |> String.trim |> unquote in
      let value =
        String.sub text (index + 1) (String.length text - index - 1)
        |> String.trim
      in
      if key = "" then raise (Parse_error (line, "mapping key cannot be empty"));
      (key, value)

let parse input =
  try
    let lines = preprocess input in
    let length = Array.length lines in
    let index = ref 0 in
    let current () = if !index < length then Some lines.(!index) else None in
    let rec node indent =
      match current () with
      | None -> `Null
      | Some line when line.indent <> indent ->
          raise
            (Parse_error
               (line.number, Printf.sprintf "expected indentation %d" indent))
      | Some line when String.starts_with ~prefix:"-" line.text ->
          sequence indent
      | Some _ -> mapping indent []
    and value_after_entry parent_indent raw =
      if String.starts_with ~prefix:"|" raw then
        block_scalar parent_indent ~folded:false raw
      else if String.starts_with ~prefix:">" raw then
        block_scalar parent_indent ~folded:true raw
      else if raw <> "" then plain_scalar parent_indent raw
      else
        match current () with
        | Some next
          when next.indent > parent_indent
               || next.indent = parent_indent
                  && String.starts_with ~prefix:"-" next.text ->
            node next.indent
        | _ -> `Null
    and plain_scalar parent_indent first =
      let values = ref [ first ] in
      let continue = ref true in
      while !continue do
        match current () with
        | Some line when line.indent > parent_indent ->
            values := line.text :: !values;
            incr index
        | _ -> continue := false
      done;
      List.rev !values |> String.concat " " |> scalar
    and block_scalar parent_indent ~folded header =
      let values = ref [] in
      let continue = ref true in
      while !continue do
        match current () with
        | Some line when line.indent > parent_indent ->
            values := line.text :: !values;
            incr index
        | _ -> continue := false
      done;
      let separator = if folded then " " else "\n" in
      let value = String.concat separator (List.rev !values) in
      if String.ends_with ~suffix:"-" header then `String value
      else `String (value ^ "\n")
    and mapping indent initial =
      let fields = ref initial in
      let continue = ref true in
      while !continue do
        match current () with
        | Some line
          when line.indent = indent
               && not (String.starts_with ~prefix:"-" line.text) ->
            let key, raw = split_pair line.number line.text in
            incr index;
            let value = value_after_entry indent raw in
            fields := (key, value) :: !fields
        | _ -> continue := false
      done;
      `Assoc (List.rev !fields)
    and sequence indent =
      let items = ref [] in
      let continue = ref true in
      while !continue do
        match current () with
        | Some line
          when line.indent = indent && String.starts_with ~prefix:"-" line.text
          ->
            let rest =
              String.sub line.text 1 (String.length line.text - 1)
              |> String.trim
            in
            incr index;
            let item =
              if rest = "" then
                match current () with
                | Some next when next.indent > indent -> node next.indent
                | _ -> `Null
              else
                match String.index_opt rest ':' with
                | None -> scalar rest
                | Some _ ->
                    let key, raw = split_pair line.number rest in
                    let logical_indent = indent + 2 in
                    let first_value = value_after_entry logical_indent raw in
                    mapping logical_indent [ (key, first_value) ]
            in
            items := item :: !items
        | _ -> continue := false
      done;
      `List (List.rev !items)
    in
    if length = 0 then Ok (`Assoc [])
    else (
      if lines.(0).text = "---" then incr index;
      if !index = length then Ok (`Assoc [])
      else
        let root_indent = lines.(!index).indent in
        let value = node root_indent in
        if !index < length && lines.(!index).text = "..." then incr index;
        if !index <> length then
          let line = lines.(!index) in
          Error
            (Printf.sprintf "line %d: %s" line.number
               (if line.text = "---" then
                  "multiple YAML documents are not supported"
                else "unexpected input"))
        else Ok value)
  with Parse_error (line, message) ->
    Error (Printf.sprintf "line %d: %s" line message)
