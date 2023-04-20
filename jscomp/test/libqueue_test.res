/* ********************************************************************* */
/*  */
/* OCaml */
/*  */
/* Jeremie Dimino, Jane Street Europe */
/*  */
/* Copyright 2015 Institut National de Recherche en Informatique et */
/* en Automatique.  All rights reserved.  This file is distributed */
/* under the terms of the Q Public License version 1.0. */
/*  */
/* ********************************************************************* */

module Q = {
  include Queue

  let to_list = q => fold((l, x) => list{x, ...l}, list{}, q) |> List.rev
}

let does_raise = (f, q) =>
  try {
    ignore((f(q): int))
    false
  } catch {
  | Q.Empty => true
  }

let () = {
  let q = Q.create()
  ()
  assert(Q.to_list(q) == list{} && Q.length(q) == 0)
  Q.add(1, q)
  assert(Q.to_list(q) == list{1} && Q.length(q) == 1)
  Q.add(2, q)
  assert(Q.to_list(q) == list{1, 2} && Q.length(q) == 2)
  Q.add(3, q)
  assert(Q.to_list(q) == list{1, 2, 3} && Q.length(q) == 3)
  Q.add(4, q)
  assert(Q.to_list(q) == list{1, 2, 3, 4} && Q.length(q) == 4)
  assert(Q.take(q) == 1)
  assert(Q.to_list(q) == list{2, 3, 4} && Q.length(q) == 3)
  assert(Q.take(q) == 2)
  assert(Q.to_list(q) == list{3, 4} && Q.length(q) == 2)
  assert(Q.take(q) == 3)
  assert(Q.to_list(q) == list{4} && Q.length(q) == 1)
  assert(Q.take(q) == 4)
  assert(Q.to_list(q) == list{} && Q.length(q) == 0)
  assert(does_raise(Q.take, q))
}

let () = {
  let q = Q.create()
  Q.add(1, q)
  assert(Q.take(q) == 1)
  assert(does_raise(Q.take, q))
  Q.add(2, q)
  assert(Q.take(q) == 2)
  assert(does_raise(Q.take, q))
  assert(Q.length(q) == 0)
}

let () = {
  let q = Q.create()
  Q.add(1, q)
  assert(Q.peek(q) == 1)
  Q.add(2, q)
  assert(Q.peek(q) == 1)
  Q.add(3, q)
  assert(Q.peek(q) == 1)
  assert(Q.peek(q) == 1)
  assert(Q.take(q) == 1)
  assert(Q.peek(q) == 2)
  assert(Q.take(q) == 2)
  assert(Q.peek(q) == 3)
  assert(Q.take(q) == 3)
  assert(does_raise(Q.peek, q))
  assert(does_raise(Q.peek, q))
}

let () = {
  let q = Q.create()
  for i in 1 to 10 {
    Q.add(i, q)
  }
  Q.clear(q)
  assert(Q.length(q) == 0)
  assert(does_raise(Q.take, q))
  assert(q == Q.create())
  Q.add(42, q)
  assert(Q.take(q) == 42)
}

let () = {
  let q1 = Q.create()
  for i in 1 to 10 {
    Q.add(i, q1)
  }
  let q2 = Q.copy(q1)
  assert(Q.to_list(q1) == list{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})
  assert(Q.to_list(q2) == list{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})
  assert(Q.length(q1) == 10)
  assert(Q.length(q2) == 10)
  for i in 1 to 10 {
    assert(Q.take(q1) == i)
  }
  for i in 1 to 10 {
    assert(Q.take(q2) == i)
  }
}

let () = {
  let q = Q.create()
  assert(Q.is_empty(q))
  for i in 1 to 10 {
    Q.add(i, q)
    assert(Q.length(q) == i)
    assert(!Q.is_empty(q))
  }
  for i in 10 downto 1 {
    assert(Q.length(q) == i)
    assert(!Q.is_empty(q))
    ignore((Q.take(q): int))
  }
  assert(Q.length(q) == 0)
  assert(Q.is_empty(q))
}

let () = {
  let q = Q.create()
  for i in 1 to 10 {
    Q.add(i, q)
  }
  let i = ref(1)
  Q.iter(j => {
    assert(i.contents == j)
    incr(i)
  }, q)
}

let () = {
  let q1 = Q.create() and q2 = Q.create()
  assert(Q.length(q1) == 0)
  assert(Q.to_list(q1) == list{})
  assert(Q.length(q2) == 0)
  assert(Q.to_list(q2) == list{})
  Q.transfer(q1, q2)
  assert(Q.length(q1) == 0)
  assert(Q.to_list(q1) == list{})
  assert(Q.length(q2) == 0)
  assert(Q.to_list(q2) == list{})
}

let () = {
  let q1 = Q.create() and q2 = Q.create()
  for i in 1 to 4 {
    Q.add(i, q1)
  }
  assert(Q.length(q1) == 4)
  assert(Q.to_list(q1) == list{1, 2, 3, 4})
  assert(Q.length(q2) == 0)
  assert(Q.to_list(q2) == list{})
  Q.transfer(q1, q2)
  assert(Q.length(q1) == 0)
  assert(Q.to_list(q1) == list{})
  assert(Q.length(q2) == 4)
  assert(Q.to_list(q2) == list{1, 2, 3, 4})
}

let () = {
  let q1 = Q.create() and q2 = Q.create()
  for i in 5 to 8 {
    Q.add(i, q2)
  }
  assert(Q.length(q1) == 0)
  assert(Q.to_list(q1) == list{})
  assert(Q.length(q2) == 4)
  assert(Q.to_list(q2) == list{5, 6, 7, 8})
  Q.transfer(q1, q2)
  assert(Q.length(q1) == 0)
  assert(Q.to_list(q1) == list{})
  assert(Q.length(q2) == 4)
  assert(Q.to_list(q2) == list{5, 6, 7, 8})
}

let () = {
  let q1 = Q.create() and q2 = Q.create()
  for i in 1 to 4 {
    Q.add(i, q1)
  }
  for i in 5 to 8 {
    Q.add(i, q2)
  }
  assert(Q.length(q1) == 4)
  assert(Q.to_list(q1) == list{1, 2, 3, 4})
  assert(Q.length(q2) == 4)
  assert(Q.to_list(q2) == list{5, 6, 7, 8})
  Q.transfer(q1, q2)
  assert(Q.length(q1) == 0)
  assert(Q.to_list(q1) == list{})
  assert(Q.length(q2) == 8)
  assert(Q.to_list(q2) == list{5, 6, 7, 8, 1, 2, 3, 4})
}

let () = print_endline("OK")
