(** Checkout [Ctype.arity] [Btype.repr] [Predef]

    Note that [f g x] can also be [(f g) x] even though we get a non-function
    return type *)

let f0 g x = g x
let f1 g x : unit = g x

module X : sig
  type t
end = struct
  type t = int -> int
end

let f2 g x : X.t = g x
let f3 g x = ignore @@ g x
let f4 g x : 'a * 'b = g x
let f5 g x : _ list = g x
let f6 g x : int = g x
let f7 g x : _ -> _ = g x

module X0 = struct type t = int -> int end

let f8 g x : X0.t = g x
let f9 g x : Abstract_type.t = g x
let f10 g x : Abstract_type.arrow_type = g x
let f11 g x : _ Abstract_type.poly_type = g x
let f12 g x : _ Abstract_type.poly_abstract_type = g x

module X2 : sig
  type t

  val f13 : ('a -> t) -> 'a -> t
end = struct
  type t = int -> int

  let f13 g x : t = g x
end

let f14 h g x : unit = h g x
