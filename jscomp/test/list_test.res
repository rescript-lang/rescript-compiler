let list_suites = {
  open Mt
  list{
    ("length", _ => Eq(1, List.length(list{(0, 1, 2, 3, 4)})) /* This is tuple haha */),
    ("length2", _ => Eq(5, List.length(list{0, 1, 2, 3, 4})) /* This is tuple haha */),
    (
      "long_length",
      _ => {
        let v = 30_000
        Eq(v, List.length(Array.to_list(Array.init(v, _ => 0))))
      },
    ),
    (
      "sort",
      _ => Eq(
        List.sort((x: int, y) => Pervasives.compare(x, y), list{4, 1, 2, 3}),
        list{1, 2, 3, 4},
      ),
    ),
    (__LOC__, _ => Eq(true, List.mem(3, list{1, 2, 3}))),
    (__LOC__, _ => Eq(false, List.mem(4, list{1, 2, 3}))),
    (__LOC__, _ => Eq(9, List.assoc(4, list{(1, 2), (4, 9)}))),
  }
}

let () = Mt.from_pair_suites(__MODULE__, list_suites)
