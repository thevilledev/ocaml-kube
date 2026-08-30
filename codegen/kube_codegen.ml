let fail message =
  prerr_endline ("kube-codegen: " ^ message);
  exit 2

let bind result fn =
  match result with
  | Ok value -> fn value
  | Error message -> fail message

let () =
  let schema_path = ref None in
  let manifest_path = ref None in
  let implementation_path = ref None in
  let interface_path = ref None in
  let schema_output_path = ref None in
  let check = ref false in
  let derive_stable_resources = ref false in
  let set target value = target := Some value in
  let arguments =
    [
      ( "--schema",
        Arg.String (set schema_path),
        "PATH Kubernetes Swagger 2.0 input" );
      ( "--manifest",
        Arg.String (set manifest_path),
        "PATH Resource selection manifest" );
      ( "--ml",
        Arg.String (set implementation_path),
        "PATH Generated implementation" );
      ("--mli", Arg.String (set interface_path), "PATH Generated interface");
      ( "--schema-output",
        Arg.String (set schema_output_path),
        "PATH Write the reproducible dependency-closure schema" );
      ( "--check",
        Arg.Set check,
        "Fail if generated files differ instead of writing" );
      ( "--derive-stable-resources",
        Arg.Set derive_stable_resources,
        "Derive and replace manifest resources from stable LIST/WATCH paths" );
    ]
  in
  Arg.parse arguments
    (fun value -> fail ("unexpected argument: " ^ value))
    "kube-codegen --schema PATH --manifest PATH --ml PATH --mli PATH [OPTIONS]";
  let required name = function
    | Some value -> value
    | None -> fail (name ^ " is required")
  in
  let schema_path = required "--schema" !schema_path in
  let manifest_path = required "--manifest" !manifest_path in
  let implementation_path = required "--ml" !implementation_path in
  let interface_path = required "--mli" !interface_path in
  bind (Openapi_codegen.load_json schema_path) (fun schema ->
      bind (Openapi_codegen.load_json manifest_path) (fun manifest ->
          let manifest =
            if !derive_stable_resources then
              Openapi_codegen.derive_stable_manifest ~schema ~manifest
            else Ok manifest
          in
          bind manifest (fun manifest ->
              bind (Openapi_codegen.generate ~schema ~manifest) (fun output ->
                  let act =
                    if !check then Openapi_codegen.check_file
                    else Openapi_codegen.write_file
                  in
                  let write_manifest next =
                    if !derive_stable_resources then
                      let contents =
                        Yojson.Safe.pretty_to_string manifest ^ "\n"
                      in
                      bind (act manifest_path contents) (fun () -> next ())
                    else next ()
                  in
                  write_manifest (fun () ->
                      bind (act implementation_path output.implementation)
                        (fun () ->
                          bind (act interface_path output.interface) (fun () ->
                              (match !schema_output_path with
                              | None -> ()
                              | Some path ->
                                  bind (act path output.pruned_schema) Fun.id);
                              Printf.printf
                                "generated %d OpenAPI definitions\n%!"
                                output.definition_count)))))))
