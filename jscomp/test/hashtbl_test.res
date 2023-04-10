open Hashtbl

let to_list = tbl => fold((k, v, acc) => list{(k, v), ...acc}, tbl, list{})

let f = () => {
  let tbl = Hashtbl.create(17)
  add(tbl, 1, '1')
  add(tbl, 2, '2')
  \"@@"(List.sort(((a: int, _), (b, _)) => compare(a, b)), to_list(tbl))
}

let g = count => {
  let tbl = Hashtbl.create(17)
  for i in 0 to count {
    replace(tbl, i * 2, string_of_int(i))
  }
  for i in 0 to count {
    replace(tbl, i * 2, string_of_int(i))
  }
  let v = to_list(tbl)
  let v = \"@@"(List.sort(((x, _), (y: int, _)) => compare(x, y)), v)
  Array.of_list(v)
}

let suites = {
  open Mt
  list{
    ("simple", _ => Eq(list{(1, '1'), (2, '2')}, f())),
    (
      "more_iterations",
      _ => {
        let count = 1000
        Eq(Array.init(count + 1, i => (2 * i, string_of_int(i))), g(count))
      },
    ),
    (
      "More_labels_regressionfix_374",
      _ => {
        let tbl: Hashtbl.t<int, int> = MoreLabels.Hashtbl.create(30)
        Hashtbl.add(tbl, 3, 3)
        Eq(Hashtbl.length(tbl), 1)
      },
    ),
  }
}

Mt.from_pair_suites(__MODULE__, suites)
