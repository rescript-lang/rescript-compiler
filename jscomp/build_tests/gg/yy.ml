type t = 
  | Foo
  [@@bs.deriving jsConverter]

let u = Xx.sum 3

