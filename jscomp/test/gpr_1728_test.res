let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => {
  incr(test_id)
  suites :=
    list{(loc ++ (" id " ++ string_of_int(test_id.contents)), _ => Mt.Eq(x, y)), ...suites.contents}
}

let foo = x => int_of_string(x) !== 3

let badInlining = obj =>
  if foo(obj["field"]) {
    ()
  }

eq(__LOC__, badInlining({"field": "3"}), ())

eq(__LOC__, int_of_string("-13"), -13)
eq(__LOC__, int_of_string("+13"), 13)
eq(__LOC__, int_of_string("13"), 13)
eq(__LOC__, int_of_string("0u32"), 32)
eq(__LOC__, int_of_string("-0u32"), -32)
eq(__LOC__, int_of_string("+0u32"), 32)
eq(__LOC__, int_of_string("+0x32"), 50)
eq(__LOC__, int_of_string("-0x32"), -50)
eq(__LOC__, int_of_string("0x32"), 50)
Mt.from_pair_suites(__MODULE__, suites.contents)
