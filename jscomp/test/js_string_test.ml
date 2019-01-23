let suites = Mt.[
    "make", (fun _ ->
      Eq("null", Js.String2.make Js.null |. Js.String2.concat "")
    );

    "fromCharCode", (fun _ ->
      Eq("a", Js.String2.fromCharCode 97)
    );
    "fromCharCodeMany", (fun _ ->
      Eq("az", Js.String2.fromCharCodeMany [| 97; 122 |])
    );

    (* es2015 *)
    "fromCodePoint", (fun _ ->
      Eq("a", Js.String2.fromCodePoint 0x61)
    );
    "fromCodePointMany", (fun _ ->
      Eq("az", Js.String2.fromCodePointMany [| 0x61; 0x7a |])
    );

    "length", (fun _ ->
      Eq(3, "foo" |. Js.String2.length)
    );

    "get", (fun _ ->
      Eq("a", Js.String2.get "foobar" 4)
    );

    "charAt", (fun _ ->
      Eq("a", "foobar" |. Js.String2.charAt 4)
    );

    "charCodeAt", (fun _ ->
      Eq(97., "foobar" |. Js.String2.charCodeAt 4)
    );

    (* es2015 *)
    "codePointAt", (fun _ ->
      Eq(Some 0x61, "foobar" |. Js.String2.codePointAt 4)
    );
    "codePointAt - out of bounds", (fun _ ->
      Eq(None, "foobar" |. Js.String2.codePointAt 98)
    );

    "concat", (fun _ ->
      Eq("foobar", "foo" |. Js.String2.concat "bar")
    );
    "concatMany", (fun _ ->
      Eq("foobarbaz", "foo" |. Js.String2.concatMany [| "bar"; "baz" |])
    );

    (* es2015 *)
    "endsWith", (fun _ ->
      Eq(true, "foobar" |. Js.String2.endsWith "bar")
    );
    "endsWithFrom", (fun _ ->
      Eq(false, "foobar" |. Js.String2.endsWithFrom "bar" 1)
    );

    (* es2015 *)
    "includes", (fun _ ->
      Eq(true, "foobarbaz" |. Js.String2.includes "bar")
    );
    "includesFrom", (fun _ ->
      Eq(false, "foobarbaz" |. Js.String2.includesFrom "bar" 4)
    );

    "indexOf", (fun _ ->
      Eq(3, "foobarbaz" |. Js.String2.indexOf "bar")
    );
    "indexOfFrom", (fun _ ->
      Eq((-1), "foobarbaz" |. Js.String2.indexOfFrom "bar" 4)
    );

    "lastIndexOf", (fun _ ->
      Eq(3, "foobarbaz" |. Js.String2.lastIndexOf "bar")
    );
    "lastIndexOfFrom", (fun _ ->
      Eq(3, "foobarbaz" |. Js.String2.lastIndexOfFrom "bar" 4)
    );

    "localeCompare", (fun _ ->
      Eq(0., "foo" |. Js.String2.localeCompare "foo")
    );

    "match", (fun _ ->
      Eq(Some [| "na"; "na" |], "banana" |. Js.String2.match_ [%re "/na+/g"])
    );
    "match - no match", (fun _ ->
      Eq(None, "banana" |. Js.String2.match_ [%re "/nanana+/g"])
    );

    (* es2015 *)
    "normalize", (fun _ ->
      Eq("foo", "foo" |. Js.String2.normalize)
    );
    "normalizeByForm", (fun _ ->
      Eq("foo", "foo" |. Js.String2.normalizeByForm "NFKD")
    );

    (* es2015 *)
    "repeat", (fun _ ->
      Eq("foofoofoo", "foo" |. Js.String2.repeat 3)
    );

    "replace", (fun _ ->
      Eq("fooBORKbaz", "foobarbaz" |. Js.String2.replace "bar" "BORK")
    );
    "replaceByRe", (fun _ ->
      Eq("fooBORKBORK", "foobarbaz" |. Js.String2.replaceByRe [%re "/ba./g"] "BORK")
    );
    "unsafeReplaceBy0", (fun _ ->
      let replace = fun whole offset s ->
        if whole = "bar" then "BORK"
        else "DORK"
      in
      Eq("fooBORKDORK", "foobarbaz" |. Js.String2.unsafeReplaceBy0 [%re "/ba./g"] replace)
    );
    "unsafeReplaceBy1", (fun _ ->
      let replace = fun whole p1 offset s ->
        if whole = "bar" then "BORK"
        else "DORK"
      in
      Eq("fooBORKDORK", "foobarbaz" |. Js.String2.unsafeReplaceBy1 [%re "/ba./g"] replace)
    );
    "unsafeReplaceBy2", (fun _ ->
      let replace = fun whole p1 p2 offset s ->
        if whole = "bar" then "BORK"
        else "DORK"
      in
      Eq("fooBORKDORK", "foobarbaz" |. Js.String2.unsafeReplaceBy2 [%re "/ba./g"] replace)
    );
    "unsafeReplaceBy3", (fun _ ->
      let replace = fun whole p1 p2 p3 offset s ->
        if whole = "bar" then "BORK"
        else "DORK"
      in
      Eq("fooBORKDORK", "foobarbaz" |. Js.String2.unsafeReplaceBy3 [%re "/ba./g"] replace)
    );

    "search", (fun _ ->
      Eq(3, "foobarbaz" |. Js.String2.search [%re "/ba./g"])
    );

    "slice", (fun _ ->
      Eq("bar", "foobarbaz" |. Js.String2.slice ~from:3 ~to_:6)
    );
    "sliceToEnd", (fun _ ->
      Eq("barbaz", "foobarbaz" |. Js.String2.sliceToEnd ~from:3)
    );

    "split", (fun _ ->
      Eq([| "foo"; "bar"; "baz" |], "foo bar baz" |. Js.String2.split " ")
    );
    "splitAtMost", (fun _ ->
      Eq([| "foo"; "bar" |], "foo bar baz" |. Js.String2.splitAtMost " " ~limit:2)
    );
    "splitByRe", (fun _ ->
      Eq([| "foo"; "bar"; "baz" |], "foo bar baz" |. Js.String2.splitByRe [%re "/\\s/"])
    );
    "splitByReAtMost", (fun _ ->
      Eq([| "foo"; "bar" |], "foo bar baz" |. Js.String2.splitByReAtMost [%re "/\\s/"] ~limit:2)
    );

    (* es2015 *)
    "startsWith", (fun _ ->
      Eq(true, "foobarbaz" |. Js.String2.startsWith "foo")
    );
    "startsWithFrom", (fun _ ->
      Eq(false, "foobarbaz" |. Js.String2.startsWithFrom "foo" 1)
    );

    "substr", (fun _ ->
      Eq("barbaz", "foobarbaz" |. Js.String2.substr ~from:3)
    );
    "substrAtMost", (fun _ ->
      Eq("bar", "foobarbaz" |. Js.String2.substrAtMost ~from:3 ~length:3)
    );

    "substring", (fun _ ->
      Eq("bar", "foobarbaz" |. Js.String2.substring ~from:3 ~to_:6)
    );
    "substringToEnd", (fun _ ->
      Eq("barbaz", "foobarbaz" |. Js.String2.substringToEnd ~from:3)
    );

    "toLowerCase", (fun _ ->
      Eq("bork", "BORK" |. Js.String2.toLowerCase)
    );
    "toLocaleLowerCase", (fun _ ->
      Eq("bork", "BORK" |. Js.String2.toLocaleLowerCase)
    );
    "toUpperCase", (fun _ ->
      Eq("FUBAR", "fubar" |. Js.String2.toUpperCase)
    );
    "toLocaleUpperCase", (fun _ ->
      Eq("FUBAR", "fubar" |. Js.String2.toLocaleUpperCase)
    );

    "trim", (fun _ ->
      Eq("foo", "  foo  " |. Js.String2.trim)
    );

    (* es2015 *)
    "anchor", (fun _ ->
      Eq("<a name=\"bar\">foo</a>", "foo" |. Js.String2.anchor "bar")
    );
    "link", (fun _ ->
      Eq("<a href=\"https://reason.ml\">foo</a>", "foo" |. Js.String2.link "https://reason.ml")
    );
    __LOC__ , (fun _ -> Ok (Js.String2.includes "ab" "a"))
]
;; Mt.from_pair_suites __MODULE__ suites
