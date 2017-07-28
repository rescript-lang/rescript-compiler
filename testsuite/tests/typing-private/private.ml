module Foobar : sig
  type t = private int
end = struct
  type t = int
end;;

module F0 : sig type t = private int end = Foobar;;

let f (x : F0.t) = (x : Foobar.t);; (* fails *)

module F = Foobar;;

let f (x : F.t) = (x : Foobar.t);;

module M = struct type t = <m:int> end;;
module M1 : sig type t = private <m:int; ..> end = M;;
module M2 :  sig type t = private <m:int; ..> end = M1;;
fun (x : M1.t) -> (x : M2.t);; (* fails *)

module M3 : sig type t = private M1.t end = M1;;
fun x -> (x : M3.t :> M1.t);;
fun x -> (x : M3.t :> M.t);;
module M4 : sig type t = private M3.t end = M2;; (* fails *)
module M4 : sig type t = private M3.t end = M;; (* fails *)
module M4 : sig type t = private M3.t end = M1;; (* might be ok *)
module M5 : sig type t = private M1.t end = M3;;
module M6 : sig type t = private < n:int; .. > end = M1;; (* fails *)

module Bar : sig type t = private Foobar.t val f : int -> t end =
  struct type t = int let f (x : int) = (x : t) end;; (* must fail *)

module M : sig
  type t = private T of int
  val mk : int -> t
end = struct
  type t = T of int
  let mk x = T(x)
end;;

module M1 : sig
  type t = M.t
  val mk : int -> t
end = struct
  type t = M.t
  let mk = M.mk
end;;

module M2 : sig
  type t = M.t
  val mk : int -> t
end = struct
  include M
end;;

module M3 : sig
  type t = M.t
  val mk : int -> t
end = M;;

module M4 : sig
    type t = M.t = T of int
    val mk : int -> t
  end = M;;
(* Error: The variant or record definition does not match that of type M.t *)

module M5 : sig
  type t = M.t = private T of int
  val mk : int -> t
end = M;;

module M6 : sig
  type t = private T of int
  val mk : int -> t
end = M;;

module M' : sig
  type t_priv = private T of int
  type t = t_priv
  val mk : int -> t
end = struct
  type t_priv = T of int
  type t = t_priv
  let mk x = T(x)
end;;

module M3' : sig
  type t = M'.t
  val mk : int -> t
end = M';;

module M : sig type 'a t = private T of 'a end =
  struct type 'a t = T of 'a end;;

module M1 : sig type 'a t = 'a M.t = private T of 'a end =
  struct type 'a t = 'a M.t = private T of 'a end;;

(* PR#6090 *)
module Test = struct type t = private A end
module Test2 : module type of Test with type t = Test.t = Test;;
let f (x : Test.t) = (x : Test2.t);;
let f Test2.A = ();;
let a = Test2.A;; (* fail *)
(* The following should fail from a semantical point of view,
   but allow it for backward compatibility *)
module Test2 : module type of Test with type t = private Test.t = Test;;

(* PR#6331 *)
type t = private < x : int; .. > as 'a;;
type t = private (< x : int; .. > as 'a) as 'a;;
type t = private < x : int > as 'a;;
type t = private (< x : int > as 'a) as 'b;;
type 'a t = private < x : int; .. > as 'a;;
type 'a t = private 'a constraint 'a = < x : int; .. >;;
