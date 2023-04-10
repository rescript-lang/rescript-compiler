let split = Ext_string_test.split

let split_by = Ext_string_test.split_by

let suites = {
  open Mt
  list{
    ("split", _ => Eq(split(~keep_empty=true, "hihi", 'i'), list{"h", "h", ""})),
    ("split_non_empty", _ => Eq(split("hihi", 'i'), list{"h", "h"})),
    ("split_empty", _ => Eq(split(~keep_empty=true, "", 'i'), list{})),
    ("split_normal", _ => Eq(split(~keep_empty=true, "h i i", ' '), list{"h", "i", "i"})),
    (
      "split_by",
      _ => Eq(
        List.filter(s => s != "", split_by(x => x == ' ' || x == '\t', "h hgso hgso \t hi")),
        list{"h", "hgso", "hgso", "hi"},
      ),
    ),
  }
}

Mt.from_pair_suites(__MODULE__, suites)
