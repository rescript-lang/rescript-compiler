type (_, _) eq = Eq : ('a, 'a) eq | Neq : int -> ('a, 'b) eq;;
type 'a t;;
let f (type a) (Neq n : (a, a t) eq) = n;;   (* warn! *)
[%%expect{|
type (_, _) eq = Eq : ('a, 'a) eq | Neq : int -> ('a, 'b) eq
type 'a t
Line _, characters 15-40:
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
Eq
val f : ('a, 'a t) eq -> int = <fun>
|}];;

module F (T : sig type _ t end) = struct
 let f (type a) (Neq n : (a, a T.t) eq) = n  (* warn! *)
end;;
[%%expect{|
Line _, characters 16-43:
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
Eq
module F :
  functor (T : sig type _ t end) -> sig val f : ('a, 'a T.t) eq -> int end
|}];;
