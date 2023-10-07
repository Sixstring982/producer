let () =
  let open Alcotest in
  run "Producers"
    [
      ( "tests",
        [
          ( test_case "Single-node graph produces a result" `Quick @@ fun () ->
            let node : (unit, int) Producer.Node.t =
              Producer.Node.make [] (fun () -> 123)
            in
            let graph = Producer.Graph.make node in
            let actual = graph.execute () in
            check int "equal" 123 actual );
          (*
           *
           *)
          ( test_case "Multi-node graph produces a result" `Quick @@ fun () ->
            let int_node : (unit, int) Producer.Node.t =
              Producer.Node.make [] (fun () -> 123)
            in
            let mul_node : (unit, int) Producer.Node.t =
              Producer.Node.make [ int_node ] (fun () n -> n * 5)
            in
            let graph = Producer.Graph.make mul_node in
            let actual = graph.execute () in
            check int "equal" (123 * 5) actual );
          (*
           *
           *)
          ( test_case "Graph with heterogeneous node types" `Quick @@ fun () ->
            let int_node : (unit, int) Producer.Node.t =
              Producer.Node.make [] (fun () -> 123)
            in
            let mul_node : (unit, int) Producer.Node.t =
              Producer.Node.make [ int_node ] (fun () n -> n * 5)
            in
            let string_node : (unit, string) Producer.Node.t =
              Producer.Node.make [ int_node; mul_node ] (fun () n mul ->
                  string_of_int n ^ " * 5 = " ^ string_of_int mul)
            in
            let graph = Producer.Graph.make string_node in
            let actual = graph.execute () in
            check string "equal" "123 * 5 = 615" actual );
          (*
           *
           *)
          ( test_case "Caches first value returned from a node" `Quick
          @@ fun () ->
            let int_node : (unit, int) Producer.Node.t =
              let call_count : int ref = { contents = 0 } in
              Producer.Node.make [] (fun () ->
                  call_count := call_count.contents + 1;
                  if call_count.contents = 1 then 123 else 420)
            in
            let mul_node : (unit, int) Producer.Node.t =
              Producer.Node.make [ int_node ] (fun () n -> n * 5)
            in
            let string_node : (unit, string) Producer.Node.t =
              Producer.Node.make [ int_node; mul_node ] (fun () n mul ->
                  string_of_int n ^ " * 5 = " ^ string_of_int mul)
            in
            let graph = Producer.Graph.make string_node in
            let actual = graph.execute () in
            check string "equal" "123 * 5 = 615" actual );
          (*
           *
           *)
          ( test_case "Works in a monadic context" `Quick @@ fun () ->
            let module Producer = Producer.Make (struct
              include Option

              let return = some
            end) in
            let int_node : (unit, int) Producer.Node.t =
              Producer.Node.make [] (fun () -> Some 123)
            in
            let other_int_node : (unit, int) Producer.Node.t =
              Producer.Node.make [] (fun () -> Some 456)
            in
            let mul_node : (unit, int) Producer.Node.t =
              Producer.Node.make [ int_node; other_int_node ] (fun () a b ->
                  Some (a * b))
            in
            let graph = Producer.Graph.make mul_node in
            let actual = graph.execute () in
            check (option int) "equal" (Some (123 * 456)) actual );
          (*
           *
           *)
          ( test_case "Obeys the monadic context" `Quick @@ fun () ->
            let module Producer = Producer.Make (struct
              include Option

              let return = some
            end) in
            let int_node : (unit, int) Producer.Node.t =
              Producer.Node.make [] (fun () -> Some 123)
            in
            let other_int_node : (unit, int) Producer.Node.t =
              Producer.Node.make [] (fun () -> None)
            in
            let mul_node : (unit, int) Producer.Node.t =
              Producer.Node.make [ int_node; other_int_node ] (fun () a b ->
                  Some (a * b))
            in
            let graph = Producer.Graph.make mul_node in
            let actual = graph.execute () in
            check (option int) "equal" None actual );
          (*
           *
           *)
          ( test_case "Works with a two-argument monad" `Quick @@ fun () ->
            let module Producer = Producer.Make2(struct
              include Result

              let return = ok
            end) in
            let int_node : (unit, int, string) Producer.Node.t =
              Producer.Node.make [] (fun () -> Ok 123)
            in
            let other_int_node : (unit, int, string) Producer.Node.t =
              Producer.Node.make [] (fun () -> Error "uh oh")
            in
            let mul_node : (unit, int, string) Producer.Node.t =
              Producer.Node.make [ int_node; other_int_node ] (fun () a b ->
                  Ok (a * b))
            in
            let graph = Producer.Graph.make mul_node in
            let actual = graph.execute () in
            check (result int string) "equal" (Error "uh oh") actual );
          (*
           *
           *)
          ( test_case "Structurally-typed context" `Quick @@ fun () ->
            let int_node : (< .. >, int) Producer.Node.t =
              Producer.Node.make [] (fun _ -> 123)
            in
            let mul_node : (< multiplier : int ; .. >, int) Producer.Node.t =
              Producer.Node.make [ int_node ] (fun c n -> c#multiplier * n)
            in
            let string_node : (< tag : string ; .. >, string) Producer.Node.t =
              Producer.Node.make [ int_node; mul_node ] (fun c n m ->
                  Format.sprintf "[%s]: int_node = %d, mul_node = %d" c#tag n m)
            in
            let graph = Producer.Graph.make string_node in
            let actual =
              graph.execute
                (object
                   method multiplier = 10
                   method tag = "TAG"
                end)
            in
            check string "equal" "[TAG]: int_node = 123, mul_node = 1230" actual
          );
        ] );
    ]
