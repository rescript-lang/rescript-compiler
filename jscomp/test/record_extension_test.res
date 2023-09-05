/* for o in jscomp/test/*test.js ; do npx mocha  $o ; done */*/

let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(~test_id, ~suites, loc, x, y)

/* Record_extension */
type t0 = ..
type t0 += Inline_record({x: int, y: string})

let f = x =>
  switch x {
  | Inline_record({x, y}) => Some(x + int_of_string(y))
  | _ => None
  }
let v0 = Inline_record({x: 3, y: "4"})

eq(__LOC__, f(v0), Some(7))

/* Record_unboxed */
type t1 = | @unboxed A({x: int})

/* Record_inlined */
type t2 =
  | B
  | C({x: int, y: string})
  | D({w: int})
let f2 = x =>
  switch x {
  | D(_)
  | B => 0

  | C({x}) => x
  }

let f2_with = x =>
  switch x {
  | D(_)
  | B => x
  | C(u) => C({...u, x: 0})
  }

exception A({name: int, x: int})
exception B(int, int)
exception C({name: int})

let u = f =>
  try f() catch {
  | A({name, x}) => name + x
  | B(a, b) => a + b
  | C(x) => x.name
  | _ => -1
  }

let () = Mt.from_pair_suites(__LOC__, suites.contents)
