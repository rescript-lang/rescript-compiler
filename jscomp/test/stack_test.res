open Stack

let to_list = v => {
  let acc = ref(list{})
  while \"@@"(not, is_empty(v)) {
    acc := list{pop(v), ...acc.contents}
  }
  List.rev(acc.contents)
}

let v = () => {
  let v = create()
  push(3, v)
  push(4, v)
  push(1, v)
  to_list(v)
}

let suites = list{("push_test", _ => Mt.Eq(list{1, 4, 3}, v()))}

Mt.from_pair_suites(__MODULE__, suites)
