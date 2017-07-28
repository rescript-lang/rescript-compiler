type ('env, 'a) var =
 | Zero : ('a * 'env, 'a) var
 | Succ : ('env, 'a) var -> ('b * 'env, 'a) var
;;
type ('env, 'a) typ =
 | Tint : ('env, int) typ
 | Tbool : ('env, bool) typ
 | Tvar : ('env, 'a) var -> ('env, 'a) typ
;;
let f : type env a. (env, a) typ -> (env, a) typ -> int = fun ta tb ->
 match ta, tb with
   | Tint, Tint -> 0
   | Tbool, Tbool -> 1
   | Tvar var, tb -> 2
;;
let x = f Tint (Tvar Zero)
;;
