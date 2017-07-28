(* A synthetic example of bootstrapped data structure
   (suggested by J-C Filliatre) *)

module type ORD = sig
  type t
  val compare : t -> t -> int
end

module type SET = sig
  type elt
  type t
  val iter : (elt -> unit) -> t -> unit
end

type 'a tree = E | N of 'a tree * 'a * 'a tree

module Bootstrap2
  (MakeDiet : functor (X: ORD) -> SET with type t = X.t tree and type elt = X.t)
  : SET with type elt = int =
struct

  type elt = int

  module rec Elt : sig
    type t = I of int * int | D of int * Diet.t * int
    val compare : t -> t -> int
    val iter : (int -> unit) -> t -> unit
  end =
  struct
    type t = I of int * int | D of int * Diet.t * int
    let compare x1 x2 = 0
    let rec iter f = function
      | I (l, r) -> for i = l to r do f i done
      | D (_, d, _) -> Diet.iter (iter f) d
  end

  and Diet : SET with type t = Elt.t tree and type elt = Elt.t = MakeDiet(Elt)

  type t = Diet.t
  let iter f = Diet.iter (Elt.iter f)
end
