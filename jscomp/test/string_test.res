let ff = x => {
  let a = switch x {
  | "0"
  | "1"
  | "2" => 3
  | "3" => 4
  | "4" => 6
  | "7" => 7
  | _ => 8
  }
  a + 3
}

let gg = x => {
  let a = switch x {
  | 0
  | 1
  | 2 => 3
  | 3 => 4
  | 4 => 6
  | 8 => 7
  | _ => 8
  }
  a + 3
}

let rev_split_by_char = (c, s) => {
  let rec loop = (i, l) =>
    try {
      let i' = String.index_from(s, i, c)
      let s' = String.sub(s, i, i' - i)
      loop(
        i' + 1,
        if s' == "" {
          l
        } else {
          list{s', ...l}
        },
      )
    } catch {
    | Not_found => list{String.sub(s, i, String.length(s) - i), ...l}
    }

  loop(0, list{})
}

let xsplit = (~delim, s) => {
  let rec loop = (l, x) =>
    switch x {
    | 0 => l
    | i =>
      switch String.rindex_from(s, i - 1, delim) {
      | i' =>
        let l = list{String.sub(s, i' + 1, i - i' - 1), ...l}
        let l = if i' == 0 {
          list{"", ...l}
        } else {
          l
        }
        loop(l, i')
      | exception Not_found => list{String.sub(s, 0, i), ...l}
      }
    }

  let len = String.length(s)
  switch len {
  | 0 => list{}
  | _ => loop(list{}, len)
  }
}

@val external string_of_char: char => string = "String.fromCharCode"

let string_of_chars = x => \"@@"(String.concat(""), List.map(string_of_char, x))

Mt.from_pair_suites(
  __MODULE__,
  list{
    ("mutliple switch", _ => Eq(9, ff("4"))),
    ("int switch", _ => Eq(9, gg(4))),
    ("escape_normal", _ => Eq("haha", String.escaped("haha"))),
    ("escape_bytes", _ => Eq(Bytes.of_string("haha"), Bytes.escaped(Bytes.of_string("haha")))),
    /* FIXME it used char pattern match */
    ("escape_quote", _ => Eq("\\\"\\\"", String.escaped(`""`))),
    ("rev_split_by_char", _ => Eq(list{"", "bbbb", "bbbb"}, rev_split_by_char('a', "bbbbabbbba"))),
    (__LOC__, _ => Eq(list{"aaaa"}, rev_split_by_char(',', "aaaa"))),
    ("xsplit", _ => Eq(list{"a", "b", "c"}, xsplit(~delim='.', "a.b.c"))),
    ("split_empty", _ => Eq(list{}, Ext_string_test.split("", '_'))),
    (
      "split_empty2",
      _ => Eq(
        list{"test_unsafe_obj_ffi_ppx.cmi"},
        Ext_string_test.split(" test_unsafe_obj_ffi_ppx.cmi", ~keep_empty=false, ' '),
      ),
    ),
    ("rfind", _ => Eq(7, Ext_string_test.rfind("__index__js", ~sub="__"))),
    ("rfind_2", _ => Eq(0, Ext_string_test.rfind("__index_js", ~sub="__"))),
    ("rfind_3", _ => Eq(-1, Ext_string_test.rfind("_index_js", ~sub="__"))),
    ("find", _ => Eq(0, Ext_string_test.find("__index__js", ~sub="__"))),
    ("find_2", _ => Eq(6, Ext_string_test.find("_index__js", ~sub="__"))),
    ("find_3", _ => Eq(-1, Ext_string_test.find("_index_js", ~sub="__"))),
    ("of_char", _ => Eq(string_of_char('0'), String.make(1, '0'))),
    ("of_chars", _ => Eq(string_of_chars(list{'0', '1', '2'}), "012")),
  },
)
