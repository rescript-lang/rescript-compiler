(* PR6216: wrong inlining of GADT match *)

type _ t =
 | Float : float t
 | String : string t

let f : type a . a t -> a -> unit = fun t a ->
 match t with
 | Float -> ()
 | String -> ignore (String.length a : int)

let _g (kind : float t) (x : float) : unit = f kind (x *. 13.)
