let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eqx = (loc, x, y) => Mt.eq_suites(~test_id, ~suites, loc, x, y)
let b = (loc, x) => Mt.bool_suites(~test_id, ~suites, loc, x)
module N = Belt.HashMap
module S = Belt.Map.Int
/* module Y = struct  
   type t = int 

   end */
let eq = (x: int, y) => x == y
let hash = (x: int) => Hashtbl.hash(x)
let cmp = (x: int, y) => compare(x, y)
module Y = unpack(Belt.Id.hashable(~eq, ~hash))
let empty: N.t<int, int, _> = N.make(~id=module(Y), ~hintSize=30)

/*
[%bs.hash {
  eq : 
  hash : 
}]
*/

module I = Array_data_util
let \"++" = Belt.Array.concat
let add = \"+"

let () = {
  N.mergeMany(empty, [(1, 1), (2, 3), (3, 3), (2, 2)])
  eqx(__LOC__, N.get(empty, 2), Some(2))
  eqx(__LOC__, N.size(empty), 3)
}

module A = Belt.Array
module So = Belt.SortArray

let () = {
  let u = \"++"(I.randomRange(30, 100), I.randomRange(40, 120))
  let v = A.zip(u, u)
  let xx = N.fromArray(~id=module(Y), v)
  eqx(__LOC__, N.size(xx), 91)
  eqx(__LOC__, So.stableSortBy(N.keysToArray(xx), cmp), I.range(30, 120))
}

let () = {
  let u = \"++"(I.randomRange(0, 100_000), I.randomRange(0, 100))
  let v = N.make(~id=module(Y), ~hintSize=40)
  N.mergeMany(v, A.zip(u, u))
  eqx(__LOC__, N.size(v), 100_001)
  for i in 0 to 1_000 {
    N.remove(v, i)
  }
  eqx(__LOC__, N.size(v), 99_000)
  for i in 0 to 2_000 {
    N.remove(v, i)
  }
  eqx(__LOC__, N.size(v), 98_000)
  b(__LOC__, A.every(I.range(2_001, 100_000), x => N.has(v, x)))
}

Mt.from_pair_suites(__MODULE__, suites.contents)
