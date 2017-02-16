let suites = Mt.[
    "make", (fun _ ->
      Eq("null", Js.String.make Js.null |> Js.String.concat "")
    );

    "fromCharCode", (fun _ ->
      Eq("a", Js.String.fromCharCode 97)
    );
    "fromCharCodeMany", (fun _ ->
      Eq("az", Js.String.fromCharCodeMany [| 97; 122 |])
    );

    (* es2015 *)
    "fromCodePoint", (fun _ ->
      Eq("a", Js.String.fromCodePoint 0x61)
    );
    "fromCodePointMany", (fun _ ->
      Eq("az", Js.String.fromCodePointMany [| 0x61; 0x7a |])
    );

    "length", (fun _ ->
      Eq(3, "foo" |> Js.String.length)
    );

    "get", (fun _ ->
      Eq("a", Js.String.get "foobar" 4)
    );

    "charAt", (fun _ ->
      Eq("a", "foobar" |> Js.String.charAt 4)
    );

    "charCodeAt", (fun _ ->
      Eq(97., "foobar" |> Js.String.charCodeAt 4)
    );

    (* es2015 *)
    "codePointAt", (fun _ ->
      Eq(Js.Undefined.return 0x61, "foobar" |> Js.String.codePointAt 4)
    );

    "concat", (fun _ ->
      Eq("foobar", "foo" |> Js.String.concat "bar")
    );
    "concatMany", (fun _ ->
      Eq("foobarbaz", "foo" |> Js.String.concatMany [| "bar"; "baz" |])
    );

    (* es2015 *)
    "endsWith", (fun _ ->
      Eq(Js.true_, "foobar" |> Js.String.endsWith "bar")
    );
    "endsWithFrom", (fun _ ->
      Eq(Js.false_, "foobar" |> Js.String.endsWithFrom "bar" 1)
    );

    (* es2015 *)
    "includes", (fun _ ->
      Eq(Js.true_, "foobarbaz" |> Js.String.includes "bar")
    );
    "includesFrom", (fun _ ->
      Eq(Js.false_, "foobarbaz" |> Js.String.includesFrom "bar" 4)
    );

    "indexOf", (fun _ ->
      Eq(3, "foobarbaz" |> Js.String.indexOf "bar")
    );
    "indexOfFrom", (fun _ ->
      Eq((-1), "foobarbaz" |> Js.String.indexOfFrom "bar" 4)
    );

    "lastIndexOf", (fun _ ->
      Eq(3, "foobarbaz" |> Js.String.lastIndexOf "bar")
    );
    "lastIndexOfFrom", (fun _ ->
      Eq(3, "foobarbaz" |> Js.String.lastIndexOfFrom "bar" 4)
    );

    "localeCompare", (fun _ ->
      Eq(0., "foo" |> Js.String.localeCompare "foo")
    );

    "match", (fun _ ->
      Eq(Js.Null.return [| "na"; "na" |], "banana" |> Js.String.match_ [%re "/na+/g"])
    );

    (* es2015 *)
    "normalize", (fun _ ->
      Eq("foo", "foo" |> Js.String.normalize)
    );
    "normalizeByForm", (fun _ ->
      Eq("foo", "foo" |> Js.String.normalizeByForm "NFKD")
    );

    (* es2015 *)
    "repeat", (fun _ ->
      Eq("foofoofoo", "foo" |> Js.String.repeat 3)
    );

    "replace", (fun _ ->
      Eq("fooBORKbaz", "foobarbaz" |> Js.String.replace "bar" "BORK")
    );
    "replaceByRe", (fun _ ->
      Eq("fooBORKBORK", "foobarbaz" |> Js.String.replaceByRe [%re "/ba./g"] "BORK")
    );

    "search", (fun _ ->
      Eq(3, "foobarbaz" |> Js.String.search [%re "/ba./g"])
    );

    "slice", (fun _ ->
      Eq("bar", "foobarbaz" |> Js.String.slice ~from:3 ~to_:6)
    );
    "sliceToEnd", (fun _ ->
      Eq("barbaz", "foobarbaz" |> Js.String.sliceToEnd ~from:3)
    );

    "split", (fun _ ->
      Eq([| "foo"; "bar"; "baz" |], "foo bar baz" |> Js.String.split " ")
    );
    "splitAtMost", (fun _ ->
      Eq([| "foo"; "bar" |], "foo bar baz" |> Js.String.splitAtMost " " ~limit:2)
    );
    "splitByRe", (fun _ ->
      Eq([| "foo"; "bar"; "baz" |], "foo bar baz" |> Js.String.splitByRe [%re "/\\s/"])
    );
    "splitByReAtMost", (fun _ ->
      Eq([| "foo"; "bar" |], "foo bar baz" |> Js.String.splitByReAtMost [%re "/\\s/"] ~limit:2)
    );

    (* es2015 *)
    "startsWith", (fun _ ->
      Eq(Js.true_, "foobarbaz" |> Js.String.startsWith "foo")
    );
    "startsWithFrom", (fun _ ->
      Eq(Js.false_, "foobarbaz" |> Js.String.startsWithFrom "foo" 1)
    );

    "substr", (fun _ ->
      Eq("barbaz", "foobarbaz" |> Js.String.substr ~from:3)
    );
    "substrAtMost", (fun _ ->
      Eq("bar", "foobarbaz" |> Js.String.substrAtMost ~from:3 ~length:3)
    );

    "substring", (fun _ ->
      Eq("bar", "foobarbaz" |> Js.String.substring ~from:3 ~to_:6)
    );
    "substringToEnd", (fun _ ->
      Eq("barbaz", "foobarbaz" |> Js.String.substringToEnd ~from:3)
    );

    "toLowerCase", (fun _ ->
      Eq("bork", "BORK" |> Js.String.toLowerCase)
    );
    "toLocaleLowerCase", (fun _ ->
      Eq("bork", "BORK" |> Js.String.toLocaleLowerCase)
    );
    "toUpperCase", (fun _ ->
      Eq("FUBAR", "fubar" |> Js.String.toUpperCase)
    );
    "toLocaleUpperCase", (fun _ ->
      Eq("FUBAR", "fubar" |> Js.String.toLocaleUpperCase)
    );

    "trim", (fun _ ->
      Eq("foo", "  foo  " |> Js.String.trim)
    );

    (* es2015 *)
    "anchor", (fun _ ->
      Eq("<a name=\"bar\">foo</a>", "foo" |> Js.String.anchor "bar")
    );
    "link", (fun _ ->
      Eq("<a href=\"https://reason.ml\">foo</a>", "foo" |> Js.String.link "https://reason.ml")
    );
]
;; Mt.from_pair_suites __FILE__ suites
