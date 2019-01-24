(* PR#5907 *)

type 'a t = 'a;;
let f (g : 'a list -> 'a t -> 'a) s = g s s;;
[%%expect{|
type 'a t = 'a
Line _, characters 42-43:
Error: This expression has type 'a list
       but an expression was expected of type 'a t = 'a
       The type variable 'a occurs inside 'a list
|}];;
let f (g : 'a * 'b -> 'a t -> 'a) s = g s s;;
[%%expect{|
Line _, characters 42-43:
Error: This expression has type 'a * 'b
       but an expression was expected of type 'a t = 'a
       The type variable 'a occurs inside 'a * 'b
|}];;
