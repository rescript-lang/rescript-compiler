
let suites = Mt.[
  "captures", (fun _ ->
    let re = [%re "/(\\d+)-(?:(\\d+))?/g"] in
    let str = "3-" in
    match re |. Js.Re2.exec str with
      | Some result ->
        let defined = (Js.Re2.captures result).(1) in
        let undefined = (Js.Re2.captures result).(2) in
        Eq((Js.Nullable.return "3", Js.Nullable.null), (defined, undefined))
      | None -> Fail()
  );

  "fromString", (fun _ ->
    (* From the example in js_re.mli *)
    let contentOf tag xmlString =
      Js.Re2.fromString ("<" ^ tag ^ ">(.*?)<\\/" ^ tag ^">")
        |. Js.Re2.exec xmlString
        |> function
          | Some result -> Js.Nullable.toOption (Js.Re2.captures result).(1)
          | None -> None in
    Eq (contentOf "div" "<div>Hi</div>", Some "Hi")
  );

  "exec_literal", (fun _ ->
    match [%re "/[^.]+/"] |. Js.Re2.exec "http://xxx.domain.com" with
    | Some res ->
      Eq(Js.Nullable.return "http://xxx", (Js.Re2.captures res).(0))
    | None ->
      FailWith "regex should match"
  );

  "exec_no_match", (fun _ ->
    match [%re "/https:\\/\\/(.*)/"] |. Js.Re2.exec "http://xxx.domain.com" with
    | Some _ ->  FailWith "regex should not match"
    | None -> Ok true
  );

  "test_str", (fun _ ->
    let res = "foo"
      |. Js.Re2.fromString
      |. Js.Re2.test "#foo#" in

    Eq(true, res)
  );

  "fromStringWithFlags", (fun _ ->
    let res = Js.Re2.fromStringWithFlags "foo" "g" in

    Eq(true, res |. Js.Re2.global)
  );
  "result_index", (fun _ ->
    match "zbar" |. Js.Re2.fromString |. Js.Re2.exec "foobarbazbar" with
    | Some res ->
      Eq(8, res |. Js.Re2.index)
    | None ->
      Fail ()
  );
  "result_input", (fun _ ->
    let input = "foobar" in

    match [%re "/foo/g"] |. Js.Re2.exec input with
    | Some res ->
      Eq(input,  res |. Js.Re2.input)
    | None ->
      Fail ()
  );

  (* es2015 *)
  "t_flags", (fun _ ->
    Eq("gi", [%re "/./ig"] |. Js.Re2.flags)
  );

  "t_global", (fun _ ->
    Eq(true, [%re "/./ig"] |. Js.Re2.global)
  );
  "t_ignoreCase", (fun _ ->
    Eq(true, [%re "/./ig"] |. Js.Re2.ignoreCase)
  );
  "t_lastIndex", (fun _ ->
    let re = [%re "/na/g"] in
    let _ = re |. Js.Re2.exec "banana" in

    Eq(4,  re |. Js.Re2.lastIndex)
  );
  "t_setLastIndex", (fun _ ->
    let re = [%re "/na/g"] in

    let before = Js.Re2.lastIndex re in
    let () = Js.Re2.setLastIndex re 42 in
    let after = Js.Re2.lastIndex re in

    Eq((0, 42),  (before, after))
  );
  "t_multiline", (fun _ ->
    Eq(false, [%re "/./ig"] |. Js.Re2.multiline)
  );
  "t_source", (fun _ ->
    Eq("f.+o", [%re "/f.+o/ig"] |. Js.Re2.source)
  );

  (* es2015 *)
  "t_sticky", (fun _ ->
    Eq(true, [%re "/./yg"] |. Js.Re2.sticky)
  );
  "t_unicode", (fun _ ->
    Eq(false, [%re "/./yg"] |. Js.Re2.unicode)
  );
]

;; Mt.from_pair_suites __MODULE__ suites
