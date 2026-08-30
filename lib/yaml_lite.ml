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

let preprocess input =
  input |> String.split_on_char '\n'
  |> List.mapi (fun index raw -> (index + 1, trim_right raw))
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

let scalar value =
  let value = String.trim value in
  match String.lowercase_ascii value with
  | "" | "null" | "~" -> `Null
  | "true" -> `Bool true
  | "false" -> `Bool false
  | "{}" -> `Assoc []
  | "[]" -> `List []
  | _ -> (
      match int_of_string_opt value with
      | Some value -> `Int value
      | None -> `String (unquote value))

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
      if raw <> "" then scalar raw
      else
        match current () with
        | Some next
          when next.indent > parent_indent
               || next.indent = parent_indent
                  && String.starts_with ~prefix:"-" next.text ->
            node next.indent
        | _ -> `Null
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
    else
      let root_indent = lines.(0).indent in
      let value = node root_indent in
      if !index <> length then
        let line = lines.(!index) in
        Error (Printf.sprintf "line %d: unexpected input" line.number)
      else Ok value
  with Parse_error (line, message) ->
    Error (Printf.sprintf "line %d: %s" line message)
