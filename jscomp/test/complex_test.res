open Complex

let suites = {
  open Mt
  list{("basic_add", _ => Eq({re: 2., im: 2.}, add(add(add(one, one), i), i)))}
}

Mt.from_pair_suites(__MODULE__, suites)
