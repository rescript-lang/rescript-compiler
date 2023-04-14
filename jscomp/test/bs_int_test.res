let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(loc, x, y, ~test_id, ~suites)
let b = (loc, x) => Mt.bool_suites(loc, x, ~test_id, ~suites)
let throw = (loc, x) => Mt.throw_suites(~test_id, ~suites, loc, x)
let neq = (loc, x, y) => {
  incr(test_id)
  suites :=
    list{
      (loc ++ (" id " ++ string_of_int(test_id.contents)), _ => Mt.Neq(x, y)),
      ...suites.contents,
    }
}

module I = Belt.Int

let () = {
  eq(__LOC__, I.toFloat(1), 1.0)
  eq(__LOC__, I.toFloat(-1), -1.0)
}

let () = {
  eq(__LOC__, I.fromFloat(1.0), 1)
  eq(__LOC__, I.fromFloat(1.3), 1)
  eq(__LOC__, I.fromFloat(1.7), 1)
  eq(__LOC__, I.fromFloat(-1.0), -1)
  eq(__LOC__, I.fromFloat(-1.5), -1)
  eq(__LOC__, I.fromFloat(-1.7), -1)
}

let () = {
  eq(__LOC__, I.fromString("1"), Some(1))
  eq(__LOC__, I.fromString("-1"), Some(-1))
  eq(__LOC__, I.fromString("1.7"), Some(1))
  eq(__LOC__, I.fromString("-1.0"), Some(-1))
  eq(__LOC__, I.fromString("-1.5"), Some(-1))
  eq(__LOC__, I.fromString("-1.7"), Some(-1))
  eq(__LOC__, I.fromString("not an int"), None)
}

let () = {
  eq(__LOC__, I.toString(1), "1")
  eq(__LOC__, I.toString(-1), "-1")
}

let () = {
  open! I
  eq(__LOC__, 2 + 3, 5)
  eq(__LOC__, 2 - 3, -1)
  eq(__LOC__, 2 * 3, 6)
  eq(__LOC__, 2 / 3, 0)
}

Mt.from_pair_suites(__LOC__, suites.contents)
