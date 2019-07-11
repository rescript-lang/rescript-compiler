(* module Mt = Mock_mt *)

;;
Mt.from_pair_suites "Closure"
  Mt.
    [ ( "partial"
      , fun _ -> Eq (Test_google_closure.(a, b, c), ("3", 101, [|1; 2|])) ) ]
