module type A = Foo with type t = string
module type A = Foo with type Bar.t = string
module type A = Foo with type t<'a> = string
module type A = Foo with type t<'a, 'b> = string
module type A = Foo with type t = 'st constraint 'st = int
module type A = Foo with type t = 'st constraint 'st = int constraint 'x = int
module type A = Foo
  with type t = 'st constraint 'st = int constraint 'x = int
   and type t = 'st constraint 'st = int constraint 'x = int
   and type t = 'st constraint 'st = int constraint 'x = int

module type A = Foo with type t := string
module type A = Foo with type t<'a> := string
module type A = Foo with type t<'a, 'b> := string
module type A = Foo with type Bar.t<'a, 'b> := string
module type A = Foo
  with type Bar.t<'a, 'b> := string
   and type Bar.t<'a, 'b> := string
   and type Bar.t<'a, 'b> := string

module type A = Foo with module Bar = Array
module type A = Foo with module Bar = Belt.Array
module type A = Foo with module X.Bar = Belt.Array
module type A = Foo
  with module X.Bar = Belt.Array
   and module X.Bar = Belt.Array
   and module X.Bar = Belt.Array

module type A = Foo with module Bar := Array
module type A = Foo with module Bar := Belt.Array
module type A = Foo
  with module X.Bar := Belt.Array
   and module X.Bar := Belt.Array
   and module X.Bar := Belt.Array

module Same = (Na: N, Nb: N): (
  (S with type number1 = Na.number) with type number2 = Nb.number
) => {
  type number1 = Na.number
  type number2 = Nb.number
  let rec sim = ((n, m)) =>
    if Na.is_zero(n) {
      Nb.is_zero(m)
    } else {
      sim((Na.pred(n), Nb.pred(m)))
    }
  let similar = ((n, m)) =>
    try sim((n, m)) catch {
    | Na.Too_small => false
    | Nb.Too_small => false
    }
}
