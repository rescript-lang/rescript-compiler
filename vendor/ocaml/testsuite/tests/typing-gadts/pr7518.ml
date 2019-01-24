type _ t = I : int t;;
let f (type a) (x : a t) (y : int) =
  match x, y with
  | I, (_:a) -> ()
;;
[%%expect{|
type _ t = I : int t
val f : 'a t -> int -> unit = <fun>
|}]

type ('a, 'b) eq = Refl : ('a, 'a) eq;;
let ok (type a b) (x : (a, b) eq) =
  match x, [] with
  | Refl, [(_ : a) | (_ : b)] -> []
;;
[%%expect{|
type ('a, 'b) eq = Refl : ('a, 'a) eq
Line _, characters 2-54:
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
(Refl, _::_::_)
Line _, characters 22-23:
Warning 12: this sub-pattern is unused.
val ok : ('a, 'b) eq -> 'c list = <fun>
|}]
let fails (type a b) (x : (a, b) eq) =
  match x, [] with
  | Refl, [(_ : a) | (_ : b)] -> []
  | Refl, [(_ : b) | (_ : a)] -> []
;;
[%%expect{|
Line _, characters 2-90:
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
(Refl, _::_::_)
Line _, characters 22-23:
Warning 12: this sub-pattern is unused.
Line _, characters 4-29:
Warning 11: this match case is unused.
val fails : ('a, 'b) eq -> 'c list = <fun>
|}]

(* branches must be unified! *)
let x = match [] with ["1"] -> 1 | [1.0] -> 2 | [1] -> 3 | _ -> 4;;
[%%expect{|
Line _, characters 35-40:
Error: This pattern matches values of type float list
       but a pattern was expected which matches values of type string list
       Type float is not compatible with type string
|}]
