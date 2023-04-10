let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => {
  incr(test_id)
  suites :=
    list{(loc ++ (" id " ++ string_of_int(test_id.contents)), _ => Mt.Eq(x, y)), ...suites.contents}
}

let ok = (loc, x) => {
  incr(test_id)
  suites :=
    list{(loc ++ (" id " ++ string_of_int(test_id.contents)), _ => Mt.Ok(x)), ...suites.contents}
}

let a = switch %external(___undefined_value) {
| None => 1
| Some(_) => 2
}

let test = () =>
  switch %external(__DEV__) {
  | Some(_) => Js.log("dev mode")
  | None => Js.log("producton mode")
  }

let test2 = () =>
  switch %external(__filename) {
  | Some(f) => Js.log(f)
  | None => Js.log("non node environment")
  }

let test3 = () =>
  if %external(__DEV__) == None {
    Js.log("production mode")
  }

let f = x => x == Js.undefined

let () = {
  ok(__LOC__, a > 0)
  eq(__LOC__, a, 1)
}

let () = Mt.from_pair_suites(__MODULE__, suites.contents)
