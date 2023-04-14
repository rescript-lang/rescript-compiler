module rec A: {
  let even: int => bool
} = {
  let even = n =>
    if n == 0 {
      true
    } else if n == 1 {
      false
    } else {
      B.odd(n - 1)
    }
}
and B: {
  let odd: int => bool
} = {
  let odd = n =>
    if n == 1 {
      true
    } else if n == 0 {
      false
    } else {
      A.even(n - 1)
    }
}

module rec AA: {
  let even: int => bool
  let x: unit => int
} = {
  let even = n =>
    if n == 0 {
      true
    } else if n == 1 {
      false
    } else {
      BB.odd(n - 1)
    }
  let x = () => BB.y() + 3
}
and BB: {
  let odd: int => bool
  let y: unit => int
} = {
  let odd = n =>
    if n == 1 {
      true
    } else if n == 0 {
      false
    } else {
      AA.even(n - 1)
    }
  let y = () => 32
}

module rec Even: {
  type t = Zero | Succ(Odd.t)
} = {
  type t = Zero | Succ(Odd.t)
}
and Odd: {
  type t = Succ(Even.t)
} = {
  type t = Succ(Even.t)
}

module rec AAA: {
  type t = Leaf(string) | Node(ASet.t)
  let compare: (t, t) => int
} = {
  type t = Leaf(string) | Node(ASet.t)
  let compare = (t1, t2) =>
    switch (t1, t2) {
    | (Leaf(s1), Leaf(s2)) => Pervasives.compare(s1, s2)
    | (Leaf(_), Node(_)) => 1
    | (Node(_), Leaf(_)) => -1
    | (Node(n1), Node(n2)) => ASet.compare(n1, n2)
    }
}
and ASet: Set.S with type elt = AAA.t = Set.Make(AAA)

let suites = {
  open Mt
  list{
    ("test1", _ => Eq((true, true, false, false), (A.even(2), AA.even(4), B.odd(2), BB.odd(4)))),
    ("test2", _ => Eq(BB.y(), 32)),
    ("test3", _ => Eq(AA.x(), 35)),
    ("test4", _ => Eq(true, A.even(2))),
    ("test4", _ => Eq(true, AA.even(4))),
    ("test5", _ => Eq(false, B.odd(2))),
    ("test6", _ => Eq(2, ASet.cardinal(ASet.of_list(list{Leaf("a"), Leaf("b"), Leaf("a")})))),
  }
}

Mt.from_pair_suites(__MODULE__, suites)
