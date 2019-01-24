(* use with [cvs update -r objvariants typing] *)

let f (x : [> ]) = x#m 3;;
let o = object method m x = x+2 end;;
f (`A o);;
let l = [`A o; `B(object method m x = x -2 method y = 3 end)];;
List.map f l;;
let g = function `A x -> x#m 3 | `B x -> x#y;;
List.map g l;;
fun x -> ignore (x=f); List.map x l;;
fun (x : [< `A of _ | `B of _] -> int) -> ignore (x=f); List.map x l;;


class cvar name =
  object
    method name = name
    method print ppf = Format.pp_print_string ppf name
  end

type var = [`Var of cvar]

class cint n =
  object
    method n = n
    method print ppf = Format.pp_print_int ppf n
  end

class ['a] cadd (e1 : 'a) (e2 : 'a) =
  object
    constraint 'a = [> ]
    method e1 = e1
    method e2 = e2
    method print ppf = Format.fprintf ppf "(%t, %t)" e1#print e2#print
  end

type 'a expr = [var | `Int of cint | `Add of 'a cadd]

type expr1 = expr1 expr

let print = Format.printf "%t@."

let e1 : expr1 = `Add (new cadd (`Var (new cvar "x")) (`Int (new cint 2)))
