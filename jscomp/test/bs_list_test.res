let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)

let eq = (loc, x, y) => Mt.eq_suites(loc, x, y, ~test_id, ~suites)
let b = (loc, x) => Mt.bool_suites(~test_id, ~suites, loc, x)
let throw = (loc, x) => Mt.throw_suites(~test_id, ~suites, loc, x)

/* module N = Belt.LinkList */
module N = Belt.List
module A = Belt.Array
module J = Js.Json
let sum = xs => {
  let v = ref(0)
  N.forEach(xs, x => v := v.contents + x)
  v.contents
}

let sum2 = (xs, ys) => {
  let v = ref(0)
  N.forEach2(xs, ys, (x, y) => v := v.contents + x + y)
  v.contents
}
let () = {
  let u = N.makeBy(5, i => i * i)

  /* N.checkInvariantInternal u ; */
  let f = i => eq(__LOC__, N.getExn(u, i), i * i)
  for i in 0 to 4 {
    f(i)
  }
  eq(__LOC__, N.map(u, i => i + 1), list{1, 2, 5, 10, 17})
  eq(__LOC__, N.getBy(list{1, 4, 3, 2}, x => mod(x, 2) == 0), Some(4))
  eq(__LOC__, N.getBy(list{1, 4, 3, 2}, x => mod(x, 5) == 0), None)
}

let () = {
  let \"=~" = eq("FLATTEN")

  \"=~"(
    {
      open N
      flatten(list{list{1}, list{2}, list{3}, list{}, makeBy(4, i => i)})
    },
    list{1, 2, 3, 0, 1, 2, 3},
  )
  \"=~"(N.flatten(list{}), list{})
  \"=~"(N.flatten(list{list{}, list{}, list{2}, list{1}, list{2}, list{}}), list{2, 1, 2})
}

let () = {
  let \"=~" = eq("CONCATMANY")
  \"=~"(
    {
      open N
      concatMany([list{1}, list{2}, list{3}, list{}, makeBy(4, i => i)])
    },
    list{1, 2, 3, 0, 1, 2, 3},
  )
  \"=~"(N.concatMany([]), list{})
  \"=~"(N.concatMany([list{}, list{}, list{2}, list{1}, list{2}, list{}]), list{2, 1, 2})
  \"=~"(N.concatMany([list{}, list{}, list{2, 3}, list{1}, list{2}, list{}]), list{2, 3, 1, 2})
  \"=~"(N.concatMany([list{1, 2, 3}]), list{1, 2, 3})
}

let () = eq(
  __LOC__,
  {
    open N
    concat(makeBy(100, i => i), makeBy(100, i => i))
  } |> N.toArray,
  {
    open A
    concat(makeBy(100, i => i), makeBy(100, i => i))
  },
)
let () = {
  let \"=~" = eq("APPEND")
  \"=~"(N.concat(list{1}, list{}), list{1})
  \"=~"(N.concat(list{}, list{1}), list{1})
}

let () = {
  let \"=~" = eq("ZIP")

  \"=~"(N.zip(list{1, 2, 3}, list{3, 4}), list{(1, 3), (2, 4)})
  \"=~"(N.zip(list{}, list{1}), list{})
  \"=~"(N.zip(list{}, list{}), list{})
  \"=~"(N.zip(list{1, 2, 3}, list{}), list{})
  \"=~"(N.zip(list{1, 2, 3}, list{2, 3, 4}), list{(1, 2), (2, 3), (3, 4)})
}

let mod2 = x => mod(x, 2) == 0
let evenIndex = (_x, i) => mod(i, 2) == 0

let () = {
  let \"=~" = eq("PARTITION")

  \"=~"(N.partition(list{1, 2, 3, 2, 3, 4}, mod2), (list{2, 2, 4}, list{1, 3, 3}))
  \"=~"(N.partition(list{2, 2, 2, 4}, mod2), (list{2, 2, 2, 4}, list{}))
  \"=~"(N.partition(list{2, 2, 2, 4}, x => !mod2(x)), (list{}, list{2, 2, 2, 4}))
  \"=~"(N.partition(list{}, mod2), (list{}, list{}))
}

let () = {
  let \"=~" = eq("UNZIP")
  \"=~"(N.unzip(list{}), (list{}, list{}))
  \"=~"(N.unzip(list{(1, 2)}), (list{1}, list{2}))
  \"=~"(N.unzip(list{(1, 2), (3, 4)}), (list{1, 3}, list{2, 4}))
}

let () = {
  let \"=~" = eq("FILTER")
  \"=~"(N.keep(list{1, 2, 3, 4}, mod2), list{2, 4})
  \"=~"(N.keep(list{1, 3, 41}, mod2), list{})
  \"=~"(N.keep(list{}, mod2), list{})
  \"=~"(N.keep(list{2, 2, 2, 4, 6}, mod2), list{2, 2, 2, 4, 6})
}

let () = {
  let \"=~" = eq("FILTER2")
  \"=~"(N.keepWithIndex(list{}, evenIndex), list{})
  \"=~"(N.keepWithIndex(list{1, 2, 3, 4}, evenIndex), list{1, 3})
  \"=~"(N.keepWithIndex(list{0, 1, 2, 3, 4, 5, 6, 7}, evenIndex), list{0, 2, 4, 6})
}

let id: int => int = x => x

let () = {
  let \"=~" = eq("MAP")
  \"=~"(N.map(N.makeBy(5, id), x => x * 2), list{0, 2, 4, 6, 8})
  \"=~"(N.map(list{}, id), list{})
  \"=~"(N.map(list{1}, x => -x), list{-1})
}
let add = (a, b) => a + b
let length_10_id = N.makeBy(10, id)
let length_8_id = N.makeBy(8, id)
let () = {
  let \"=~" = eq("MAP2")
  let b = length_10_id
  let c = length_8_id
  let d = N.makeBy(10, x => 2 * x)
  let map2_add = (x, y) => N.zipBy(x, y, add)
  \"=~"(map2_add(length_10_id, b), d)
  \"=~"(map2_add(list{}, list{1}), list{})
  \"=~"(map2_add(list{1}, list{}), list{})
  \"=~"(map2_add(list{}, list{}), list{})
  \"=~"(
    map2_add(length_10_id, b),
    {
      open N
      concat(map(c, x => x * 2), list{16, 18})
    },
  )
  \"=~"(
    map2_add(length_10_id, length_8_id),
    {
      open N
      mapWithIndex(length_8_id, (i, x) => i + x)
    },
  )
  \"=~"(N.reverse(N.mapReverse2(length_10_id, length_10_id, add)), N.map(length_10_id, x => x * 2))
  let xs = N.reverse(N.mapReverse2(length_8_id, length_10_id, add))
  eq(__LOC__, N.length(xs), 8)
  \"=~"(xs, N.zipBy(length_10_id, length_8_id, add))
  \"=~"(N.mapReverse2(list{1, 2, 3}, list{1, 2}, (x, y) => x + y), list{4, 2})
}

let () = {
  let \"=~" = eq("TAKE")
  \"=~"(N.take(list{1, 2, 3}, 2), Some(list{1, 2}))
  \"=~"(N.take(list{}, 1), None)
  \"=~"(N.take(list{1, 2}, 3), None)
  \"=~"(N.take(list{1, 2}, 2), Some(list{1, 2}))
  \"=~"(N.take(length_10_id, 8), Some(length_8_id))
  \"=~"(N.take(length_10_id, 0), Some(list{}))
  \"=~"(N.take(length_8_id, -2), None)
}

let () = {
  let \"=~" = eq("DROP")
  \"=~"(N.drop(length_10_id, 10), Some(list{}))
  \"=~"(N.drop(length_10_id, 8), Some(list{8, 9}))
  \"=~"(N.drop(length_10_id, 0), Some(length_10_id))
  \"=~"(N.drop(length_8_id, -1), None)
}

let () = {
  let \"=~" = eq("SPLIT")
  let a = N.makeBy(5, id)
  \"=~"(N.splitAt(list{}, 1), None)
  \"=~"(N.splitAt(a, 6), None)
  \"=~"(N.splitAt(a, 5), Some(a, list{}))
  \"=~"(N.splitAt(a, 4), Some(list{0, 1, 2, 3}, list{4}))
  \"=~"(N.splitAt(a, 3), Some(list{0, 1, 2}, list{3, 4}))
  \"=~"(N.splitAt(a, 2), Some(list{0, 1}, list{2, 3, 4}))
  \"=~"(N.splitAt(a, 1), Some(list{0}, list{1, 2, 3, 4}))
  \"=~"(N.splitAt(a, 0), Some(list{}, a))
  \"=~"(N.splitAt(a, -1), None)
}
let succx = x => x + 1

let () = {
  let \"=~" = eq("REMOVEASSOQ")
  let eqx = (x, y) => (x: int) == y
  b(__LOC__, N.hasAssoc(list{(1, "1"), (2, "2"), (3, "3")}, 2, \"="))
  b(__LOC__, !N.hasAssoc(list{(1, "1"), (2, "2"), (3, "3")}, 4, \"="))
  b(__LOC__, N.hasAssoc(list{(1, "1"), (2, "2"), (3, "3")}, 4, (x, y) => x + 1 == y))
  \"=~"(N.removeAssoc(list{(1, "1"), (2, "2"), (3, "3")}, 3, \"="), list{(1, "1"), (2, "2")})
  \"=~"(N.removeAssoc(list{(1, "1"), (2, "2"), (3, "3")}, 1, \"="), list{(2, "2"), (3, "3")})
  \"=~"(N.removeAssoc(list{(1, "1"), (2, "2"), (3, "3")}, 2, \"="), list{(1, "1"), (3, "3")})
  \"=~"(
    N.removeAssoc(list{(1, "1"), (2, "2"), (3, "3")}, 0, \"="),
    list{(1, "1"), (2, "2"), (3, "3")},
  )

  \"=~"(N.removeAssoc(list{(1, "1"), (2, "2"), (3, "3")}, 3, eqx), list{(1, "1"), (2, "2")})
  \"=~"(N.removeAssoc(list{(1, "1"), (2, "2"), (3, "3")}, 1, eqx), list{(2, "2"), (3, "3")})
  \"=~"(N.removeAssoc(list{(1, "1"), (2, "2"), (3, "3")}, 2, eqx), list{(1, "1"), (3, "3")})
  \"=~"(N.removeAssoc(list{}, 2, eqx), list{})
  let ll = list{(1, "1"), (2, "2"), (3, "3")}
  let ll0 = N.removeAssoc(ll, 0, eqx)
  b(__LOC__, ll === ll0)
  let ll1 = N.setAssoc(ll, 2, "22", \"=")
  eq(__LOC__, ll1, list{(1, "1"), (2, "22"), (3, "3")})
  let ll2 = N.setAssoc(ll1, 22, "2", \"=")
  b(__LOC__, ll2 == list{(22, "2"), ...ll1})
  b(__LOC__, N.tailExn(ll2) === ll1)
  b(
    __LOC__,
    N.setAssoc(list{(1, "a"), (2, "b"), (3, "c")}, 2, "x", \"=") ==
      list{(1, "a"), (2, "x"), (3, "c")},
  )
  b(
    __LOC__,
    N.setAssoc(list{(1, "a"), (3, "c")}, 2, "2", \"=") == list{(2, "2"), (1, "a"), (3, "c")},
  )
  eq(__LOC__, N.setAssoc(list{}, 1, "1", \"="), list{(1, "1")})
  %debugger
  eq(__LOC__, N.setAssoc(list{(1, "2")}, 1, "1", \"="), list{(1, "1")})

  eq(__LOC__, N.setAssoc(list{(0, "0"), (1, "2")}, 1, "1", \"="), list{(0, "0"), (1, "1")})
  b(__LOC__, N.getAssoc(list{(1, "a"), (2, "b"), (3, "c")}, 2, \"=") == Some("b"))
  b(__LOC__, N.getAssoc(list{(1, "a"), (2, "b"), (3, "c")}, 4, \"=") == None)
}

let () = {
  eq(
    __LOC__,
    {
      open N
      (head(length_10_id), tail(length_10_id))
    },
    (Some(0), N.drop(length_10_id, 1)),
  )
  eq(__LOC__, N.head(list{}), None)
  throw(__LOC__, _ => N.headExn(list{}))
  throw(__LOC__, _ => N.tailExn(list{}) |> ignore)
  throw(__LOC__, _ => N.getExn(list{0, 1}, -1) |> ignore)
  throw(__LOC__, _ => N.getExn(list{0, 1}, 2) |> ignore)
  eq(__LOC__, N.map(list{0, 1}, i => N.getExn(list{0, 1}, i)), list{0, 1})
  eq(__LOC__, N.headExn(list{1}), 1)
  eq(__LOC__, N.tailExn(list{1}), list{})
  N.forEachWithIndex(length_10_id, (i, x) => eq(__LOC__, N.get(length_10_id, i), Some(x)))
  eq(__LOC__, N.tail(list{}), None)
  eq(__LOC__, N.drop(list{}, 3), None)
  eq(__LOC__, N.mapWithIndex(list{}, (i, x) => i + x), list{})
  eq(__LOC__, N.get(length_10_id, -1), None)
  eq(__LOC__, N.get(length_10_id, 12), None)
  eq(__LOC__, sum(list{}), 0)
  eq(__LOC__, sum(length_10_id), 45)
  eq(__LOC__, N.makeBy(0, id), list{})
  eq(
    __LOC__,
    {
      open N
      reverse(reverse(length_10_id))
    },
    length_10_id,
  )
  eq(
    __LOC__,
    {
      open N
      reverse(reverse(length_8_id))
    },
    length_8_id,
  )
  eq(__LOC__, N.reverse(list{}), list{})
  eq(__LOC__, N.reverse(N.mapReverse(length_10_id, succx)), N.map(length_10_id, succx))
  eq(__LOC__, N.reduce(length_10_id, 0, add), 45)
  eq(__LOC__, N.reduceReverse(length_10_id, 0, add), 45)
  eq(__LOC__, N.reduceReverse(N.makeBy(10_000, i => i), 0, \"+"), 0 + 9_999 * 5_000)
  /* eq __LOC__
   (N.mapRev2 length_10_id length_8_id add ) */
  eq(__LOC__, sum2(length_10_id, length_10_id), 90)
  eq(__LOC__, sum2(length_8_id, length_10_id), 56)
  eq(__LOC__, sum2(length_10_id, length_8_id), 56)
  eq(__LOC__, N.reduce2(length_10_id, length_8_id, 0, (acc, x, y) => acc + x + y), 56)
  eq(__LOC__, N.reduce2(list{1, 2, 3}, list{2, 4, 6}, 0, (a, b, c) => a + b + c), 18)
  eq(__LOC__, N.reduceReverse2(length_10_id, length_8_id, 0, (acc, x, y) => acc + x + y), 56)
  eq(__LOC__, N.reduceReverse2(length_10_id, length_10_id, 0, (acc, x, y) => acc + x + y), 90)
  eq(__LOC__, N.reduceReverse2(list{1, 2, 3}, list{1, 2}, 0, (acc, x, y) => acc + x + y), 6)
  eq(__LOC__, N.every(list{2, 4, 6}, mod2), true)
  eq(__LOC__, N.every(list{1}, mod2), false)
  eq(__LOC__, N.every(list{}, mod2), true)
  eq(__LOC__, N.some(list{1, 2, 5}, mod2), true)
  eq(__LOC__, N.some(list{1, 3, 5}, mod2), false)
  eq(__LOC__, N.some(list{}, mod2), false)
  eq(__LOC__, N.has(list{1, 2, 3}, "2", (x, s) => string_of_int(x) == s), true)
  eq(__LOC__, N.has(list{1, 2, 3}, "0", (x, s) => string_of_int(x) == s), false)

  b(__LOC__, N.reduceReverse(list{1, 2, 3, 4}, 0, \"+") == 10)
  b(__LOC__, N.reduceReverse(list{1, 2, 3, 4}, 10, \"-") == 0)
  b(__LOC__, N.reduceReverse(list{1, 2, 3, 4}, list{}, N.add) == list{1, 2, 3, 4})
  b(__LOC__, N.reduce(list{1, 2, 3, 4}, 0, \"+") == 10)
  b(__LOC__, N.reduce(list{1, 2, 3, 4}, 10, \"-") == 0)
  b(__LOC__, N.reduce(list{1, 2, 3, 4}, list{}, N.add) == list{4, 3, 2, 1})
  b(__LOC__, N.reduceWithIndex(list{1, 2, 3, 4}, 0, (acc, x, i) => acc + x + i) == 16)
  b(__LOC__, N.reduceReverse2(list{1, 2, 3}, list{1, 2}, 0, (acc, x, y) => acc + x + y) == 6)
  let a = N.makeBy(10_000, i => i)
  b(
    __LOC__,
    N.reduceReverse2(a, list{0, ...a}, 0, (acc, x, y) => acc + x + y) == 9_999 * 10_000 - 9999,
  )
}

let () = {
  eq(__LOC__, N.every2(list{}, list{1}, (x, y) => x > y), true)
  eq(__LOC__, N.every2(list{2, 3}, list{1}, (x, y) => x > y), true)
  eq(__LOC__, N.every2(list{2}, list{1}, (x, y) => x > y), true)
  eq(__LOC__, N.every2(list{2, 3}, list{1, 4}, (x, y) => x > y), false)
  eq(__LOC__, N.every2(list{2, 3}, list{1, 0}, (x, y) => x > y), true)
  eq(__LOC__, N.some2(list{}, list{1}, (x, y) => x > y), false)
  eq(__LOC__, N.some2(list{2, 3}, list{1}, (x, y) => x > y), true)
  eq(__LOC__, N.some2(list{2, 3}, list{1, 4}, (x, y) => x > y), true)
  eq(__LOC__, N.some2(list{0, 3}, list{1, 4}, (x, y) => x > y), false)
  eq(__LOC__, N.some2(list{0, 3}, list{3, 2}, (x, y) => x > y), true)
  eq(__LOC__, N.some2(list{1, 2, 3}, list{-1, -2}, (x, y) => x == y), false)
}

let makeTest = n => eq(__LOC__, N.make(n, 3), N.makeBy(n, _ => 3))

let () = eq(__LOC__, list{}->N.add(3)->N.add(2), list{2, 3})
let () = {
  b(__LOC__, N.cmp(list{1, 2, 3}, list{0, 1, 2, 3}, compare) > 0)
  b(__LOC__, N.cmp(list{1, 2, 3, 4}, list{1, 2, 3}, compare) > 0)
  b(__LOC__, N.cmp(list{1, 2, 3}, list{1, 2, 3, 4}, compare) < 0)
  b(__LOC__, N.cmp(list{1, 2, 3}, list{0, 1, 2}, (x, y) => compare(x, y)) > 0)
  b(__LOC__, N.cmp(list{1, 2, 3}, list{1, 2, 3}, (x, y) => compare(x, y)) == 0)
  b(__LOC__, N.cmp(list{1, 2, 4}, list{1, 2, 3}, (x, y) => compare(x, y)) > 0)
  b(__LOC__, N.cmpByLength(list{}, list{}) == 0)
  b(__LOC__, N.cmpByLength(list{1}, list{}) > 0)
  b(__LOC__, N.cmpByLength(list{}, list{1}) < 0)
  b(__LOC__, N.cmpByLength(list{1, 2}, list{1}) > 0)
  b(__LOC__, N.cmpByLength(list{1}, list{1, 2}) < 0)
  b(__LOC__, N.cmpByLength(list{1, 3}, list{1, 2}) == 0)
}

let () = {
  makeTest(0)
  makeTest(1)
  makeTest(2)
  makeTest(3)
}

let () = {
  let \"=~" = eq("SORT")
  let cmp = (a, b) => a - b
  \"=~"(N.sort(list{5, 4, 3, 2}, cmp), list{2, 3, 4, 5})
  \"=~"(N.sort(list{3, 9, 37, 3, 1}, cmp), list{1, 3, 3, 9, 37})
}

let () = {
  b(__LOC__, \"@@"(not, N.eq(list{1, 2, 3}, list{1, 2}, (x, y) => x == y)))
  b(__LOC__, N.eq(list{1, 2, 3}, list{1, 2, 3}, (x, y) => x == y))
  b(__LOC__, \"@@"(not, N.eq(list{1, 2, 3}, list{1, 2, 4}, (x, y) => x == y)))
  b(__LOC__, \"@@"(not, N.eq(list{1, 2, 3}, list{1, 2, 3, 4}, \"=")))
}
let () = {
  let u0 = N.makeBy(20, x => x)
  let u1 = u0->N.keepMap(x =>
    if mod(x, 7) == 0 {
      Some(x + 1)
    } else {
      None
    }
  )
  eq(__LOC__, u1, list{1, 8, 15})
  b(
    __LOC__,
    {
      open N

      list{1, 2, 3, 4}->keepMap(x =>
        if mod(x, 2) == 0 {
          Some(-x)
        } else {
          None
        }
      ) == list{-2, -4}
    },
  )
  b(
    __LOC__,
    N.keepMap(list{1, 2, 3, 4}, x =>
      if mod(x, 5) == 0 {
        Some(x)
      } else {
        None
      }
    ) == list{},
  )
}
Mt.from_pair_suites(__MODULE__, suites.contents)
