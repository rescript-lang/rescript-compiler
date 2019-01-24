type 'a t = T of 'a
type 'a s = S of 'a

type (_, _) eq = Refl : ('a, 'a) eq;;

let f : (int s, int t) eq -> unit = function Refl -> ();;

module M (S : sig type 'a t = T of 'a type 'a s = T of 'a end) =
struct let f : ('a S.s, 'a S.t) eq -> unit = function Refl -> () end;;
[%%expect{|
type 'a t = T of 'a
type 'a s = S of 'a
type (_, _) eq = Refl : ('a, 'a) eq
Line _, characters 45-49:
Error: This pattern matches values of type (int s, int s) eq
       but a pattern was expected which matches values of type
         (int s, int t) eq
       Type int s is not compatible with type int t
|}];;
