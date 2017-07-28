type 'a t = [`A of 'a t t] as 'a;; (* fails *)

type 'a t = [`A of 'a t t];; (* fails *)

type 'a t = [`A of 'a t t] constraint 'a = 'a t;;

type 'a t = [`A of 'a t] constraint 'a = 'a t;;

type 'a t = [`A of 'a] as 'a;;

type 'a v = [`A of u v] constraint 'a = t and t = u and u = t;; (* fails *)

type 'a t = 'a;;
let f (x : 'a t as 'a) = ();; (* fails *)

let f (x : 'a t) (y : 'a) = x = y;;

(* PR#6505 *)
module type PR6505 = sig
  type 'o is_an_object = < .. > as 'o
  and 'o abs constraint 'o = 'o is_an_object
  val abs : 'o is_an_object -> 'o abs
  val unabs : 'o abs -> 'o
end;; (* fails *)
