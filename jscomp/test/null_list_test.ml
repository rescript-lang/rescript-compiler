
#if 0 then
(* https://github.com/ocaml/ocaml/pull/234 *)

type (_,_) t =
  | [] : ('a, unit) t
  | (::) : 'a * ('a, 'b) t -> ('a, unit -> 'b) t

let rec map: type a b l. (a -> b) -> (a, l) t -> (b, l) t = fun f -> function
  | [] -> []
  | h :: t -> f h :: map f t

let [a; b; c] = map succ [1;2;3]
(* This raises in our generated code *)

type t = true
type f = false
type u = ()
type l = []

let v : u = (() : unit) (* compile error *)
#end