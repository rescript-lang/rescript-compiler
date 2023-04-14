let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => {
  incr(test_id)
  suites :=
    list{(loc ++ (" id " ++ string_of_int(test_id.contents)), _ => Mt.Eq(x, y)), ...suites.contents}
}

let () = {
  eq(
    __LOC__,
    Js.List.flatten(list{list{1, 2}, list{3}, list{}, list{1, 2, 3}}),
    list{1, 2, 3, 1, 2, 3},
  )
  eq(__LOC__, Js.List.filterMap((. x) =>
      if mod(x, 2) == 0 {
        Some(x)
      } else {
        None
      }
    , list{1, 2, 3, 4, 5, 6, 7}), list{2, 4, 6})
  eq(__LOC__, Js.List.filterMap((. x) =>
      if mod(x, 2) == 0 {
        Some(x)
      } else {
        None
      }
    , list{1, 2, 3, 4, 5, 6}), list{2, 4, 6})
  eq(__LOC__, Js.List.countBy((. x) => mod(x, 2) == 0, list{1, 2, 3, 4, 5, 6}), 3)
  let v = Js.List.init(10_0000, (. i) => i)
  eq(__LOC__, Js.List.countBy((. x) => mod(x, 2) == 0, v), 50_000)
  let vv = Js.List.foldRight((. x, y) => Js.List.cons(x, y), v, list{})
  eq(__LOC__, true, Js.List.equal((. x, y) => x == y, v, vv))

  let vvv = Js.List.filter((. x) => mod(x, 10) == 0, vv)
  eq(__LOC__, Js.List.length(vvv), 10000)
  eq(__LOC__, true, Js.List.equal((. x, y) => x == y, vvv, Js.List.init(10_000, (. x) => x * 10)))
}

Mt.from_pair_suites(__MODULE__, suites.contents)
