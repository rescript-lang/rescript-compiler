(* PR#5907 *)

type 'a t = 'a;;
let f (g : 'a list -> 'a t -> 'a) s = g s s;;
let f (g : 'a * 'b -> 'a t -> 'a) s = g s s;;
