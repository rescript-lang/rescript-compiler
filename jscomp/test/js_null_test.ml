open Js_null

let suites =
  Mt.
    [ ("toOption - empty", fun _ -> Eq (None, empty |> toOption))
    ; ("toOption - 'a", fun _ -> Eq (Some (), return () |> toOption))
    ; ("return", fun _ -> Eq (Some "something", return "something" |> toOption))
    ; ("test - empty", fun _ -> Eq (true, empty = Js.null))
    ; ("test - 'a", fun _ -> Eq (false, return () = empty))
    ; ("bind - empty", fun _ -> StrictEq (empty, bind empty (fun [@bs] v -> v)))
    ; ( "bind - 'a"
      , fun _ -> StrictEq (return 4, bind (return 2) (fun [@bs] n -> n * 2)) )
    ; ( "iter - empty"
      , fun _ ->
          let hit = ref false in
          let _ = iter empty (fun [@bs] _ -> hit := true) in
          Eq (false, !hit) )
    ; ( "iter - 'a"
      , fun _ ->
          let hit = ref 0 in
          let _ = iter (return 2) (fun [@bs] v -> hit := v) in
          Eq (2, !hit) )
    ; ("fromOption - None", fun _ -> Eq (empty, None |> fromOption))
    ; ("fromOption - Some", fun _ -> Eq (return 2, Some 2 |> fromOption)) ]

;;
Mt.from_pair_suites __MODULE__ suites
