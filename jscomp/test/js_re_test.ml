
let suites = Mt.[
  "exec_literal", (fun _ ->

    let res = [%re "/[^.]+/"]
      |> Js.Re.exec "http://xxx.domain.com"
      |> Js.Re.matches in

    Eq ("xxx", Array.get res 0 |> Js.String.substringToEnd 7)

  );
  "test_str", (fun _ ->

    let res = "foo"
      |> Js.Re.fromString
      |> Js.Re.test "#foo#" in

    Eq(Js.true_, res)

  );
  "fromStringWithFlags", (fun _ ->
    let res = Js.Re.fromStringWithFlags "foo" "g" in
    Eq(Js.true_, res |> Js.Re.global)
  );
  "result_index", (fun _ ->

    let res = "zbar"
      |> Js.Re.fromString
      |> Js.Re.exec "foobarbazbar" in

    Eq(8, res |> Js.Re.index)

  );
  "result_input", (fun _ ->
    Eq("foobar", [%re "/foo/g"] |> Js.Re.exec "foobar" |> Js.Re.input)
  );
  (* Travis uses Node v4, this is es2015
  "t_flags", (fun _ ->
    Eq("gi", [%re "/./ig"] |> Js.Re.flags)
  );
  *)
  "t_global", (fun _ ->
    Eq(Js.true_, [%re "/./ig"] |> Js.Re.global)
  );
  "t_ignoreCase", (fun _ ->
    Eq(Js.true_, [%re "/./ig"] |> Js.Re.ignoreCase)
  );
  "t_lastIndex", (fun _ ->
    let re = [%re "/na/g"] in
    let _ = re |> Js.Re.exec "banana" in
    Eq(4,  re |> Js.Re.lastIndex)
  );
  "t_multiline", (fun _ ->
    Eq(Js.false_, [%re "/./ig"] |> Js.Re.multiline)
  );
  "t_source", (fun _ ->
    Eq("f.+o", [%re "/f.+o/ig"] |> Js.Re.source)
  );
  (* Travis uses Node v4, these are es2015
  "t_sticky", (fun _ ->
    Eq(Js.true_, [%re "/./yg"] |> Js.Re.sticky)
  );
  "t_unicode", (fun _ ->
    Eq(Js.false_, [%re "/./yg"] |> Js.Re.unicode)
  );
  *)
]

;; Mt.from_pair_suites __FILE__ suites
