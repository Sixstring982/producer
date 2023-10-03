let () =
  let open Alcotest in
  run "Producers" [ ("foo", [ (test_case "bar" `Quick @@ fun () -> ()) ]) ]
