/* ********************************************************************* */
/*  */
/* OCaml */
/*  */
/* Jeremie Dimino, Jane Street Europe */
/*  */
/* Copyright 2015 Institut National de Recherche en Informatique et */
/* en Automatique.  All rights reserved.  This file is distributed */
/* under the terms of the S Public License version 1.0. */
/*  */
/* ********************************************************************* */

let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (f, (a, b)) => Mt_global.collect_eq(test_id, suites, f, a, b)
let assert_ = (loc, v) => eq(loc, (v, true))

module S = {
  include Stack

  let to_list = s => {
    let l = ref(list{})
    iter(x => l := list{x, ...l.contents}, s)
    l.contents
  } /* from bottom to top */
}

let does_raise = (f, s) =>
  try {
    ignore((f(s): int))
    false
  } catch {
  | S.Empty => true
  }

let () = {
  let s = S.create()
  ()
  assert_(__LOC__, S.to_list(s) == list{} && S.length(s) == 0)
  S.push(1, s)
  assert_(__LOC__, S.to_list(s) == list{1} && S.length(s) == 1)
  S.push(2, s)
  assert_(__LOC__, S.to_list(s) == list{1, 2} && S.length(s) == 2)
  S.push(3, s)
  assert_(__LOC__, S.to_list(s) == list{1, 2, 3} && S.length(s) == 3)
  S.push(4, s)
  assert_(__LOC__, S.to_list(s) == list{1, 2, 3, 4} && S.length(s) == 4)
  assert_(__LOC__, S.pop(s) == 4)
  assert_(__LOC__, S.to_list(s) == list{1, 2, 3} && S.length(s) == 3)
  assert_(__LOC__, S.pop(s) == 3)
  assert_(__LOC__, S.to_list(s) == list{1, 2} && S.length(s) == 2)
  assert_(__LOC__, S.pop(s) == 2)
  assert_(__LOC__, S.to_list(s) == list{1} && S.length(s) == 1)
  assert_(__LOC__, S.pop(s) == 1)
  assert_(__LOC__, S.to_list(s) == list{} && S.length(s) == 0)
  assert_(__LOC__, does_raise(S.pop, s))
}

let () = {
  let s = S.create()
  S.push(1, s)
  assert_(__LOC__, S.pop(s) == 1)
  assert_(__LOC__, does_raise(S.pop, s))
  S.push(2, s)
  assert_(__LOC__, S.pop(s) == 2)
  assert_(__LOC__, does_raise(S.pop, s))
  assert_(__LOC__, S.length(s) == 0)
}

let () = {
  let s = S.create()
  S.push(1, s)
  assert_(__LOC__, S.top(s) == 1)
  S.push(2, s)
  assert_(__LOC__, S.top(s) == 2)
  S.push(3, s)
  assert_(__LOC__, S.top(s) == 3)
  assert_(__LOC__, S.top(s) == 3)
  assert_(__LOC__, S.pop(s) == 3)
  assert_(__LOC__, S.top(s) == 2)
  assert_(__LOC__, S.pop(s) == 2)
  assert_(__LOC__, S.top(s) == 1)
  assert_(__LOC__, S.pop(s) == 1)
  assert_(__LOC__, does_raise(S.top, s))
  assert_(__LOC__, does_raise(S.top, s))
}

let () = {
  let s = S.create()
  for i in 1 to 10 {
    S.push(i, s)
  }
  S.clear(s)
  assert_(__LOC__, S.length(s) == 0)
  assert_(__LOC__, does_raise(S.pop, s))
  assert_(__LOC__, s == S.create())
  S.push(42, s)
  assert_(__LOC__, S.pop(s) == 42)
}

let () = {
  let s1 = S.create()
  for i in 1 to 10 {
    S.push(i, s1)
  }
  let s2 = S.copy(s1)
  assert_(__LOC__, S.to_list(s1) == list{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})
  assert_(__LOC__, S.to_list(s2) == list{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})
  assert_(__LOC__, S.length(s1) == 10)
  assert_(__LOC__, S.length(s2) == 10)
  for i in 10 downto 1 {
    assert_(__LOC__, S.pop(s1) == i)
  }
  for i in 10 downto 1 {
    assert_(__LOC__, S.pop(s2) == i)
  }
}

let () = {
  let s = S.create()
  assert_(__LOC__, S.is_empty(s))
  for i in 1 to 10 {
    S.push(i, s)
    assert_(__LOC__, S.length(s) == i)
    assert_(__LOC__, !S.is_empty(s))
  }
  for i in 10 downto 1 {
    assert_(__LOC__, S.length(s) == i)
    assert_(__LOC__, !S.is_empty(s))
    ignore((S.pop(s): int))
  }
  assert_(__LOC__, S.length(s) == 0)
  assert_(__LOC__, S.is_empty(s))
}

let () = {
  let s = S.create()
  for i in 10 downto 1 {
    S.push(i, s)
  }
  let i = ref(1)
  S.iter(j => {
    assert_(__LOC__, i.contents == j)
    incr(i)
  }, s)
}

let () = {
  let s1 = S.create()
  assert_(__LOC__, S.length(s1) == 0)
  assert_(__LOC__, S.to_list(s1) == list{})
  let s2 = S.copy(s1)
  assert_(__LOC__, S.length(s1) == 0)
  assert_(__LOC__, S.to_list(s1) == list{})
  assert_(__LOC__, S.length(s2) == 0)
  assert_(__LOC__, S.to_list(s2) == list{})
}

let () = {
  let s1 = S.create()
  let _s2 = S.create()
  for i in 1 to 4 {
    S.push(i, s1)
  }
  assert_(__LOC__, S.length(s1) == 4)
  assert_(__LOC__, S.to_list(s1) == list{1, 2, 3, 4})
  let s2 = S.copy(s1)
  assert_(__LOC__, S.length(s1) == 4)
  assert_(__LOC__, S.to_list(s1) == list{1, 2, 3, 4})
  assert_(__LOC__, S.length(s2) == 4)
  assert_(__LOC__, S.to_list(s2) == list{1, 2, 3, 4})
}

let () = Mt.from_pair_suites(__MODULE__, suites.contents)
