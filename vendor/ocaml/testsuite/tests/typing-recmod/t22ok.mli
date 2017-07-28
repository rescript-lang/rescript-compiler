module rec A :
  sig type t = Leaf of int | Node of ASet.t val compare : t -> t -> int end
and ASet : Set.S with type elt = A.t
module Fib : sig val f : int -> int end
module After : sig val x : int end
module Before : sig val x : int end
module Strengthen : sig type t val f : t -> t end
module Strengthen2 :
  sig
    type t
    val f : t -> t
    module M : sig type u end
    module R : sig type v end
  end
module PolyRec :
  sig
    type 'a t = Leaf of 'a | Node of 'a list t * 'a list t
    val depth : 'a t -> int
  end
module StringSet : Set.S with type elt = string
module rec Expr :
  sig
    type t =
        Var of string
      | Const of int
      | Add of t * t
      | Binding of Binding.t * t
    val make_let : string -> t -> t -> t
    val fv : t -> StringSet.t
    val simpl : t -> t
  end
and Binding :
  sig
    type t = (string * Expr.t) list
    val fv : t -> StringSet.t
    val bv : t -> StringSet.t
    val simpl : t -> t
  end
module type ORDERED =
  sig
    type t
    val eq : t -> t -> bool
    val lt : t -> t -> bool
    val leq : t -> t -> bool
  end
module type HEAP =
  sig
    module Elem : ORDERED
    type heap
    val empty : heap
    val isEmpty : heap -> bool
    val insert : Elem.t -> heap -> heap
    val merge : heap -> heap -> heap
    val findMin : heap -> Elem.t
    val deleteMin : heap -> heap
  end
module Bootstrap :
  functor
    (MakeH : functor (Element : ORDERED) ->
               sig
                 module Elem :
                   sig
                     type t = Element.t
                     val eq : t -> t -> bool
                     val lt : t -> t -> bool
                     val leq : t -> t -> bool
                   end
                 type heap
                 val empty : heap
                 val isEmpty : heap -> bool
                 val insert : Elem.t -> heap -> heap
                 val merge : heap -> heap -> heap
                 val findMin : heap -> Elem.t
                 val deleteMin : heap -> heap
               end) ->
    functor (Element : ORDERED) ->
      sig
        module Elem :
          sig
            type t = Element.t
            val eq : t -> t -> bool
            val lt : t -> t -> bool
            val leq : t -> t -> bool
          end
        type heap
        val empty : heap
        val isEmpty : heap -> bool
        val insert : Elem.t -> heap -> heap
        val merge : heap -> heap -> heap
        val findMin : heap -> Elem.t
        val deleteMin : heap -> heap
      end
module LeftistHeap :
  functor (Element : ORDERED) ->
    sig
      module Elem :
        sig
          type t = Element.t
          val eq : t -> t -> bool
          val lt : t -> t -> bool
          val leq : t -> t -> bool
        end
      type heap
      val empty : heap
      val isEmpty : heap -> bool
      val insert : Elem.t -> heap -> heap
      val merge : heap -> heap -> heap
      val findMin : heap -> Elem.t
      val deleteMin : heap -> heap
    end
module Ints :
  sig
    type t = int
    val eq : 'a -> 'a -> bool
    val lt : 'a -> 'a -> bool
    val leq : 'a -> 'a -> bool
  end
module C :
  sig
    module Elem :
      sig
        type t = Ints.t
        val eq : t -> t -> bool
        val lt : t -> t -> bool
        val leq : t -> t -> bool
      end
    type heap = Bootstrap(LeftistHeap)(Ints).heap
    val empty : heap
    val isEmpty : heap -> bool
    val insert : Elem.t -> heap -> heap
    val merge : heap -> heap -> heap
    val findMin : heap -> Elem.t
    val deleteMin : heap -> heap
  end
