type t = A | B | C | D | E

let toEnum = x =>
  switch x {
  | A => 0
  | B => 1
  | C => 2
  | D => 3
  | E => 4
  }

let toString = x =>
  switch x {
  | A => "A"
  | B => "B"
  | C => "C"
  | D => "D"
  | E => "E"
  }

let bar = x =>
  switch x {
  | A => 10
  | B | C | D => 0
  | E => 10
  }

type b = True | False

let and_ = (x, y) =>
  switch (x, y) {
  | (True, False) => False
  | (False, True) => False
  | (False, False) => False
  | (True, True) => True
  }

let id = x =>
  switch x {
  | True => True
  | False => False
  }

let not_ = x =>
  switch x {
  | True => False
  | False => True
  }

type state =
  | Empty
  | Int1(int)
  | Int2(int)
let st = state =>
  switch state {
  | Empty => 0
  | Int2(intValue)
  | Int1(intValue) => 23
  }

type show = No | After(int) | Yes

let showToJs = x =>
  switch x {
  | Yes | After(_) => true
  | No => false
  }

let third = l =>
  switch l {
  | list{1, 2, 3} => true
  | _ => false
  }

type rec lst = Empty | Cons(int, lst)

let third2 = l =>
  switch l {
  | Cons(1, Cons(2, Cons(3, Empty))) => true
  | _ => false
  }

module CustomizeTags = {
  type t = | @as("dd") A | B | C | @as("qq") D(int) | E(int)

  let foo = x =>
    switch x {
    | A => 1
    | B => 2
    | C => 3
    | D(_) => 4
    | E(_) => 5
    }

  let a = A
  let b = B
  let c = C
  let d = D(42)
}
