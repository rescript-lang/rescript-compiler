open Js_null_undefined

let suites = Mt.[
  "toOption - null", (fun _ -> Eq(None, null |> toOption));
  "toOption - undefined", (fun _ -> Eq(None, undefined |> toOption));
  "toOption - empty", (fun _ -> Eq(None, undefined |> toOption));
  __LOC__, (fun _ -> Eq(Some "foo", return "foo" |> toOption));
  "return", (fun _ -> Eq(Some "something", return "something" |> toOption));
  "test - null", (fun _ -> Eq(true, null |> isNullable));
  "test - undefined", (fun _ -> Eq(true, undefined |> isNullable));
  "test - empty", (fun _ -> Eq(true, undefined |> isNullable));
  __LOC__, (fun _ -> Eq(true, return () |> isNullable));
  "bind - null", (fun _ -> StrictEq(null, bind null ((fun v -> v) [@bs])));
  "bind - undefined", (fun _ -> StrictEq(undefined, bind undefined ((fun v -> v) [@bs])));
  "bind - empty", (fun _ -> StrictEq(undefined, bind undefined ((fun v -> v) [@bs])));
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
    let _ = iter undefined ((fun _ -> hit := true) [@bs]) in
    Eq(false, !hit)
  );
  "iter - 'a", (fun _ ->
    let hit = ref 0 in
    let _ = iter (return 2) ((fun v -> hit := v) [@bs]) in
    Eq(2, !hit)
  );
  "fromOption - None", (fun _ -> Eq(undefined, None |> fromOption));
  "fromOption - Some", (fun _ -> Eq(return 2, Some 2 |> fromOption));
  "null <> undefined", (fun _ -> Ok(null <> undefined));
  "null <> empty", (fun _ -> Ok(null <> undefined));
  "undefined = empty", (fun _ -> Ok(undefined = undefined));
  __LOC__, (fun _ -> 
    Ok(
      let null =3 in 
      not (Js.isNullable (Js.Nullable.return null ))
    )
  )
]
;; Mt.from_pair_suites __MODULE__ suites
