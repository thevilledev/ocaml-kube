module K = Kube

let test_token_file () =
  let path = Filename.temp_file "kube-portability" ".token" in
  let output = open_out_bin path in
  output_string output "portable-token\n";
  close_out output;
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      let config =
        K.Config.make ~credential:(K.Config.Token_file path)
          (Uri.of_string "https://kubernetes.test")
      in
      match
        K.Config.with_authorization_secret config (fun ~origin value ->
            ( origin,
              Option.map
                (fun secret ->
                  Secret.equal_string secret "Bearer portable-token")
                value ))
      with
      | Error message -> Alcotest.fail message
      | Ok (`Protected, Some true) -> ()
      | Ok _ -> Alcotest.fail "token file did not stay on the protected path")

let test_cancel_wakeup () =
  let cancel = K.Cancel.create () in
  let waiting = Atomic.make false in
  let result = Atomic.make None in
  let thread =
    Thread.create
      (fun () ->
        Atomic.set waiting true;
        Atomic.set result (Some (K.Cancel.sleep cancel 30.)))
      ()
  in
  while not (Atomic.get waiting) do
    Thread.yield ()
  done;
  K.Cancel.cancel cancel;
  Thread.join thread;
  Alcotest.(check (option bool))
    "cancelled promptly" (Some false) (Atomic.get result)

let () =
  Alcotest.run "portability"
    [
      ( "platform",
        [
          Alcotest.test_case "protected token file" `Quick test_token_file;
          Alcotest.test_case "cancellation wakeup" `Quick test_cancel_wakeup;
        ] );
    ]
