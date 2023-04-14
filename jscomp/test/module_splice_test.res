let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, (x, y)) => {
  incr(test_id)
  suites :=
    list{(loc ++ (" id " ++ string_of_int(test_id.contents)), _ => Mt.Eq(x, y)), ...suites.contents}
}

@module @variadic external joinClasses: array<int> => int = "./joinClasses"

let a = joinClasses([1, 2, 3])

let () = {
  let pair = (a, 6)
  Js.log(pair)
  eq(__LOC__, pair)
}

let () = Mt.from_pair_suites(__MODULE__, suites.contents)
