(* Report from Jeremy Yallop *)
module F (S : sig type 'a s end) = struct
  include S
  type _ t = T : 'a -> 'a s t
end;; (* fail *)
[%%expect{|
Line _, characters 2-29:
Error: In this definition, a type variable cannot be deduced
       from the type parameters.
|}];;
(*
module M = F (struct type 'a s = int end) ;;
let M.T x = M.T 3 in x = true;;
*)

(* Fix it using #-annotations *)
(*
module F (S : sig type #'a s end) = struct
  include S
  type _ t = T : 'a -> 'a s t
end;; (* syntax error *)
module M = F (struct type 'a s = int end) ;; (* fail *)
module M = F (struct type 'a s = new int end) ;; (* ok *)
let M.T x = M.T 3 in x = true;; (* fail *)
let M.T x = M.T 3 in x = 3;; (* ok *)
*)

(* Another version using OCaml 2.00 objects *)
module F(T:sig type 'a t end) = struct
  class ['a] c x =
    object constraint 'a = 'b T.t val x' : 'b = x method x = x' end
end;; (* fail *)
[%%expect{|
Line _, characters 2-86:
Error: In this definition, a type variable cannot be deduced
       from the type parameters.
|}];;

(* Another (more direct) instance using polymorphic variants *)
(* PR#6275 *)
type 'x t = A of 'a constraint 'x = [< `X of 'a ] ;; (* fail *)
let magic (x : int) : bool  =
  let A x = A x in
  x;; (* fail *)
[%%expect{|
Line _, characters 0-49:
Error: In this definition, a type variable cannot be deduced
       from the type parameters.
|}];;

type 'a t = A : 'a -> [< `X of 'a ] t;; (* fail *)
[%%expect{|
Line _, characters 0-37:
Error: In this definition, a type variable cannot be deduced
       from the type parameters.
|}];;

(* It is not OK to allow modules exported by other compilation units *)
type (_,_) eq = Eq : ('a,'a) eq;;
let eq = Obj.magic Eq;;
(* pretend that Queue.t is not injective *)
let eq : ('a Queue.t, 'b Queue.t) eq = eq;;
type _ t = T : 'a -> 'a Queue.t t;; (* fail *)
[%%expect{|
type (_, _) eq = Eq : ('a, 'a) eq
val eq : 'a = <poly>
val eq : ('a Queue.t, 'b Queue.t) eq = Eq
Line _, characters 0-33:
Error: In this definition, a type variable cannot be deduced
       from the type parameters.
|}];;
(*
let castT (type a) (type b) (x : a t) (e: (a, b) eq) : b t =
  let Eq = e in (x : b t);;
let T (x : bool) = castT (T 3) eq;; (* we found a contradiction *)
*)

(* The following signature should not be accepted *)
module type S = sig
  type 'a s
  type _ t = T : 'a -> 'a s t
end;; (* fail *)
[%%expect{|
Line _, characters 2-29:
Error: In this definition, a type variable cannot be deduced
       from the type parameters.
|}];;
(* Otherwise we can write the following *)
module rec M : (S with type 'a s = unit) = M;;
[%%expect{|
Line _, characters 16-17:
Error: Unbound module type S
|}];;
(* For the above reason, we cannot allow the abstract declaration
   of s and the definition of t to be in the same module, as
   we could create the signature using [module type of ...] *)


(* Another problem with variance *)
(*
module M = struct type 'a t = 'a -> unit end;;
module F(X:sig type #'a t end) =
  struct type +'a s = S of 'b constraint 'a = 'b X.t end;; (* fail *)
module N = F(M);;
let o = N.S (object end);;
let N.S o' = (o :> <m : int> M.t N.s);; (* unsound! *)
*)

(* And yet another *)
type 'a q = Q;;
type +'a t = 'b constraint 'a = 'b q;;
[%%expect{|
type 'a q = Q
Line _, characters 0-36:
Error: In this definition, a type variable has a variance that
       cannot be deduced from the type parameters.
       It was expected to be unrestricted, but it is covariant.
|}];;
(* shoud fail: we do not know for sure the variance of Queue.t *)

type +'a t = T of 'a;;
type +'a s = 'b constraint 'a = 'b t;; (* ok *)
[%%expect{|
type 'a t = T of 'a
type +'a s = 'b constraint 'a = 'b t
|}];;
type -'a s = 'b constraint 'a = 'b t;; (* fail *)
[%%expect{|
Line _, characters 0-36:
Error: In this definition, a type variable has a variance that
       is not reflected by its occurrence in type parameters.
       It was expected to be contravariant, but it is covariant.
|}];;
type +'a u = 'a t;;
type 'a t = T of ('a -> 'a);;
type -'a s = 'b constraint 'a = 'b t;; (* ok *)
[%%expect{|
type 'a u = 'a t
type 'a t = T of ('a -> 'a)
type -'a s = 'b constraint 'a = 'b t
|}];;
type +'a s = 'b constraint 'a = 'b q t;; (* ok *)
[%%expect{|
type +'a s = 'b constraint 'a = 'b q t
|}];;
type +'a s = 'b constraint 'a = 'b t q;; (* fail *)
[%%expect{|
Line _, characters 0-38:
Error: In this definition, a type variable has a variance that
       cannot be deduced from the type parameters.
       It was expected to be unrestricted, but it is covariant.
|}];;


(* the problem from lablgtk2 *)
(*
module Gobject = struct
  type -'a obj
end
open Gobject;;

class virtual ['a] item_container =
 object
   constraint 'a = < as_item : [>`widget] obj; .. >
   method virtual add : 'a -> unit
 end;;
*)

(* Another variance anomaly, should not expand t in g before checking *)
type +'a t = unit constraint 'a = 'b list;;
type _ g = G : 'a -> 'a t g;; (* fail *)
[%%expect{|
type +'a t = unit constraint 'a = 'b list
Line _, characters 0-27:
Error: In this definition, a type variable cannot be deduced
       from the type parameters.
|}];;
