let (test_id, suites) = (ref(0), ref(list{}))
let eq = loc => Mt_global.collect_eq(test_id, suites, loc)
let approx = loc => Mt_global.collect_approx(test_id, suites, loc)

let bigint_compare = (x: bigint, y) => Pervasives.compare(x, y)
let generic_compare = Pervasives.compare
let bigint_equal = (x: bigint, y) => x == y
let generic_equal = \"="
let bigint_notequal = (x: bigint, y) => x != y
let generic_notequal = \"<>"
let bigint_lessthan = (x: bigint, y) => x < y
let generic_lessthan = \"<"
let bigint_greaterthan = (x: bigint, y) => x > y
let generic_greaterthan = \">"
let bigint_lessequal = (x: bigint, y) => x <= y
let generic_lessequal = \"<="
let bigint_greaterequal = (x: bigint, y) => x >= y
let generic_greaterequal = \">="

let () = {
  eq(__LOC__, bigint_compare(Js.Bigint._NaN, Js.Bigint._NaN), 0)
  eq(__LOC__, generic_compare(Js.Bigint._NaN, Js.Bigint._NaN), 0)
  eq(__LOC__, bigint_compare(Js.Bigint._NaN, -1n), -1)
  eq(__LOC__, generic_compare(Js.Bigint._NaN, -1n), -1)
  eq(__LOC__, bigint_compare(-1n, Js.Bigint._NaN), 1)
  eq(__LOC__, generic_compare(-1n, Js.Bigint._NaN), 1)
  eq(__LOC__, bigint_equal(Js.Bigint._NaN, Js.Bigint._NaN), false)
  eq(__LOC__, generic_equal(Js.Bigint._NaN, Js.Bigint._NaN), false)
  eq(__LOC__, bigint_equal(1n, Js.Bigint._NaN), false)
  eq(__LOC__, generic_equal(1n, Js.Bigint._NaN), false)
  eq(__LOC__, bigint_equal(Js.Bigint._NaN, 1n), false)
  eq(__LOC__, generic_equal(Js.Bigint._NaN, 1n), false)
  eq(__LOC__, bigint_notequal(Js.Bigint._NaN, Js.Bigint._NaN), true)
  eq(__LOC__, generic_notequal(Js.Bigint._NaN, Js.Bigint._NaN), true)
  eq(__LOC__, bigint_notequal(1n, Js.Bigint._NaN), true)
  eq(__LOC__, generic_notequal(1n, Js.Bigint._NaN), true)
  eq(__LOC__, bigint_notequal(Js.Bigint._NaN, 1n), true)
  eq(__LOC__, generic_notequal(Js.Bigint._NaN, 1n), true)
  eq(__LOC__, bigint_lessthan(Js.Bigint._NaN, Js.Bigint._NaN), false)
  eq(__LOC__, generic_lessthan(Js.Bigint._NaN, Js.Bigint._NaN), false)
  eq(__LOC__, bigint_lessthan(1n, Js.Bigint._NaN), false)
  eq(__LOC__, generic_lessthan(1n, Js.Bigint._NaN), false)
  eq(__LOC__, bigint_lessthan(Js.Bigint._NaN, 1n), false)
  eq(__LOC__, generic_lessthan(Js.Bigint._NaN, 1n), false)
  eq(__LOC__, bigint_greaterthan(Js.Bigint._NaN, Js.Bigint._NaN), false)
  eq(__LOC__, generic_greaterthan(Js.Bigint._NaN, Js.Bigint._NaN), false)
  eq(__LOC__, bigint_greaterthan(1n, Js.Bigint._NaN), false)
  eq(__LOC__, generic_greaterthan(1n, Js.Bigint._NaN), false)
  eq(__LOC__, bigint_greaterthan(Js.Bigint._NaN, 1n), false)
  eq(__LOC__, generic_greaterthan(Js.Bigint._NaN, 1n), false)
  eq(__LOC__, bigint_lessequal(Js.Bigint._NaN, Js.Bigint._NaN), false)
  eq(__LOC__, generic_lessequal(Js.Bigint._NaN, Js.Bigint._NaN), false)
  eq(__LOC__, bigint_lessequal(1n, Js.Bigint._NaN), false)
  eq(__LOC__, generic_lessequal(1n, Js.Bigint._NaN), false)
  eq(__LOC__, bigint_lessequal(Js.Bigint._NaN, 1n), false)
  eq(__LOC__, generic_lessequal(Js.Bigint._NaN, 1n), false)
  eq(__LOC__, bigint_greaterequal(Js.Bigint._NaN, Js.Bigint._NaN), false)
  eq(__LOC__, generic_greaterequal(Js.Bigint._NaN, Js.Bigint._NaN), false)
  eq(__LOC__, bigint_greaterequal(1n, Js.Bigint._NaN), false)
  eq(__LOC__, generic_greaterequal(1n, Js.Bigint._NaN), false)
  eq(__LOC__, bigint_greaterequal(Js.Bigint._NaN, 1n), false)
  eq(__LOC__, generic_greaterequal(Js.Bigint._NaN, 1n), false)
}
