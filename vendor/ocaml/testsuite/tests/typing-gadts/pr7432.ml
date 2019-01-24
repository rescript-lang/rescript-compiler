#labels false;;
type (_,_) eql = Refl : ('a, 'a) eql
type s = x:int -> y:float -> unit
type t = y:int -> x:float -> unit
type silly = {silly: 'a.'a};;
let eql : (s, t) eql = Refl;;
[%%expect{|
type (_, _) eql = Refl : ('a, 'a) eql
type s = x:int -> y:float -> unit
type t = y:int -> x:float -> unit
type silly = { silly : 'a. 'a; }
val eql : (s, t) eql = Refl
|}]

#labels true;;
let f : [`L of (s, t) eql | `R of silly] -> 'a =
  function `R {silly} -> silly
;;
[%%expect{|
Line _, characters 2-30:
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
`L Refl
val f : [ `L of (s, t) eql | `R of silly ] -> 'a = <fun>
|}]

(* Segfault: let () = print_endline (f (`L eql)) *)
