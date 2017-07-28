type 'a t = T of 'a
type 'a s = S of 'a

type (_, _) eq = Refl : ('a, 'a) eq;;

let f : (int s, int t) eq -> unit = function Refl -> ();;

module M (S : sig type 'a t = T of 'a type 'a s = T of 'a end) =
struct let f : ('a S.s, 'a S.t) eq -> unit = function Refl -> () end;;
