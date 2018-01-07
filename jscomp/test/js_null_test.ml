open Js_null

let suites = Mt.[
  "to_opt - empty", (fun _ -> Eq(None, empty |> toOption));
  "to_opt - 'a", (fun _ -> Eq(Some (), return () |> toOption));
  "return", (fun _ -> Eq(Some "something", return "something" |> toOption));
  "test - empty", (fun _ -> Eq(true, empty |> test));
  "test - 'a", (fun _ -> Eq(false, return () |> test));
  "bind - empty", (fun _ -> StrictEq(empty, bind empty ((fun v -> v) [@bs])));
  "bind - 'a", (fun _ -> StrictEq(return 4, bind (return 2) ((fun n -> n * 2) [@bs])));
  "iter - empty", (fun _ ->
    let hit = ref false in
    let _ = iter empty ((fun _ -> hit := true) [@bs]) in
    Eq(false, !hit)
  );
  "iter - 'a", (fun _ ->
    let hit = ref 0 in
    let _ = iter (return 2) ((fun v -> hit := v) [@bs]) in
    Eq(2, !hit)
  );
  "from_opt - None", (fun _ -> Eq(empty, None |> fromOption));
  "from_opt - Some", (fun _ -> Eq(return 2, Some 2 |> fromOption));
]
;; Mt.from_pair_suites __FILE__ suites
