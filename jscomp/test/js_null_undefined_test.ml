open Js_null_undefined

let suites = Mt.[
  "to_opt - null", (fun _ -> Eq(None, null |> to_opt));
  "to_opt - undefined", (fun _ -> Eq(None, undefined |> to_opt));
  "to_opt - empty", (fun _ -> Eq(None, empty |> to_opt));
  "to_opt - 'a", (fun _ -> Eq(Some "foo", return "foo" |> to_opt));
  "return", (fun _ -> Eq(Some "something", return "something" |> to_opt));
  "test - null", (fun _ -> Eq(true, null |> test));
  "test - undefined", (fun _ -> Eq(true, undefined |> test));
  "test - empty", (fun _ -> Eq(true, empty |> test));
  "test - 'a", (fun _ -> Eq(false, return () |> test));
  "bind - null", (fun _ -> StrictEq(null, bind null ((fun v -> v) [@bs])));
  "bind - undefined", (fun _ -> StrictEq(undefined, bind undefined ((fun v -> v) [@bs])));
  "bind - empty", (fun _ -> StrictEq(undefined, bind empty ((fun v -> v) [@bs])));
  "bind - 'a", (fun _ -> Eq(return 4, bind (return 2) ((fun n -> n * 2) [@bs])));
  "iter - null", (fun _ ->
    let hit = ref false in
    let _ = iter null ((fun _ -> hit := true) [@bs]) in
    Eq(false, !hit)
  );
  "iter - undefined", (fun _ ->
    let hit = ref false in
    let _ = iter undefined ((fun _ -> hit := true) [@bs]) in
    Eq(false, !hit)
  );
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
  "from_opt - None", (fun _ -> Eq(undefined, None |> from_opt));
  "from_opt - Some", (fun _ -> Eq(return 2, Some 2 |> from_opt));
  "null <> undefined", (fun _ -> Ok(null <> undefined));
  "null <> empty", (fun _ -> Ok(null <> empty));
  "undefined = empty", (fun _ -> Ok(undefined = empty));
  __LOC__, (fun _ -> 
    Ok(
      let null =3 in 
      not (Js.nullable (Js.Nullable.return null ))
    )
  )
]
;; Mt.from_pair_suites __FILE__ suites
