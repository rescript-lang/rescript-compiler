let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => {
  incr(test_id)
  suites :=
    list{(loc ++ (" id " ++ string_of_int(test_id.contents)), _ => Mt.Eq(x, y)), ...suites.contents}
}

let f = x => {
  print_string("f")
  x
}
let g = x => Int64.logor(x, f(x))

let v = ref(0)
let f2 = x => {
  incr(v)
  x
}

let g2 = x => Int64.logor(x, f2(x))

/* TODO: should be shared for small integers,
    also we should not inline it (more allocations here)
 */
let a = g2 /* 1L */(Int64.one)

let () = eq(__LOC__, v.contents, 1)
let () = Mt.from_pair_suites(__MODULE__, suites.contents)
