let fail message =
  prerr_endline ("ocaml-kube: " ^ message);
  exit 2

let scaffold argv =
  let crd = ref None in
  let output = ref None in
  let project_name = ref None in
  let version = ref None in
  let deny_raw = ref false in
  let set target value = target := Some value in
  let arguments =
    [
      ("--crd", Arg.String (set crd), "PATH apiextensions.k8s.io/v1 CRD");
      ("--output", Arg.String (set output), "DIR New project directory");
      ("--name", Arg.String (set project_name), "NAME Generated package name");
      ("--version", Arg.String (set version), "VERSION CRD version to type");
      ( "--deny-raw",
        Arg.Set deny_raw,
        "Fail when any schema subtree needs dynamic JSON" );
    ]
  in
  let current = ref 1 in
  (try
     Arg.parse_argv ~current argv arguments
       (fun value -> raise (Arg.Bad ("unexpected argument: " ^ value)))
       "ocaml-kube scaffold --crd PATH --output DIR [OPTIONS]"
   with
  | Arg.Bad message -> fail message
  | Arg.Help message ->
      print_string message;
      exit 0);
  let required name = function
    | Some value -> value
    | None -> fail (name ^ " is required")
  in
  let crd = required "--crd" !crd in
  let output = required "--output" !output in
  match Kube_scaffold.load_crd ?version:!version crd with
  | Error message -> fail message
  | Ok definition -> (
      let diagnostics = Kube_scaffold.diagnostics definition in
      List.iter
        (fun (diagnostic : Kube_scaffold.diagnostic) ->
          Printf.eprintf "ocaml-kube: warning: %s: %s\n%!" diagnostic.path
            diagnostic.reason)
        diagnostics;
      if !deny_raw && diagnostics <> [] then
        fail
          (Printf.sprintf
             "%d schema subtree(s) require dynamic JSON while --deny-raw is set"
             (List.length diagnostics));
      match
        Kube_scaffold.generate ?project_name:!project_name ~output definition
      with
      | Error message -> fail message
      | Ok paths ->
          Printf.printf "generated %s (%d files)\n%!" output (List.length paths)
      )

let init argv =
  let output = ref None in
  let group = ref None in
  let kind = ref None in
  let project_name = ref None in
  let version = ref "v1alpha1" in
  let plural = ref None in
  let singular = ref None in
  let scope = ref "namespaced" in
  let set target value = target := Some value in
  let arguments =
    [
      ("--output", Arg.String (set output), "DIR New project directory");
      ("--group", Arg.String (set group), "GROUP Kubernetes API group");
      ("--kind", Arg.String (set kind), "KIND Kubernetes resource Kind");
      ("--name", Arg.String (set project_name), "NAME Generated package name");
      ("--version", Arg.Set_string version, "VERSION API version (v1alpha1)");
      ("--plural", Arg.String (set plural), "PLURAL API resource plural");
      ("--singular", Arg.String (set singular), "SINGULAR API resource singular");
      ( "--scope",
        Arg.Set_string scope,
        "SCOPE namespaced or cluster (namespaced)" );
    ]
  in
  let current = ref 1 in
  (try
     Arg.parse_argv ~current argv arguments
       (fun value -> raise (Arg.Bad ("unexpected argument: " ^ value)))
       "ocaml-kube init --output DIR --group GROUP --kind KIND [OPTIONS]"
   with
  | Arg.Bad message -> fail message
  | Arg.Help message ->
      print_string message;
      exit 0);
  let required name = function
    | Some value -> value
    | None -> fail (name ^ " is required")
  in
  let output = required "--output" !output in
  let group = required "--group" !group in
  let kind = required "--kind" !kind in
  let scope =
    match String.lowercase_ascii (String.trim !scope) with
    | "namespaced" -> Kube_scaffold.Namespaced
    | "cluster" -> Kube_scaffold.Cluster
    | value -> fail ("--scope must be namespaced or cluster, got " ^ value)
  in
  match
    Kube_scaffold.generate_init ?project_name:!project_name ~version:!version
      ?plural:!plural ?singular:!singular ~scope ~output ~group ~kind ()
  with
  | Error message -> fail message
  | Ok paths ->
      Printf.printf "initialized %s (%d files)\n%!" output (List.length paths)

let usage () =
  prerr_endline
    "usage: ocaml-kube COMMAND [OPTIONS]\n\n\
     Commands:\n\
    \  init      Create a type-first OCaml operator project\n\
    \  scaffold  Generate an OCaml operator project from a CRD";
  exit 2

let () =
  if Array.length Sys.argv < 2 then usage ()
  else
    match Sys.argv.(1) with
    | "init" -> init Sys.argv
    | "scaffold" -> scaffold Sys.argv
    | "--help" | "-help" | "-h" ->
        print_endline
          "ocaml-kube\n\n\
           Commands:\n\
          \  init      Create a type-first OCaml operator project\n\
          \  scaffold  Generate an OCaml operator project from a CRD";
        exit 0
    | command -> fail ("unknown command: " ^ command)
