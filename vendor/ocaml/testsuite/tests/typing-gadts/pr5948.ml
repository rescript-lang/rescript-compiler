type tag = [`TagA | `TagB | `TagC];;

type 'a poly =
    AandBTags : [< `TagA of int | `TagB ] poly
  | ATag : [< `TagA of int] poly
(* constraint 'a = [< `TagA of int | `TagB] *)
;;

let intA = function `TagA i -> i
let intB = function `TagB -> 4
;;

let intAorB = function
    `TagA i -> i
  | `TagB -> 4
;;

type _ wrapPoly =
    WrapPoly : 'a poly -> ([< `TagA of int | `TagB] as 'a) wrapPoly
;;

let example6 : type a. a wrapPoly -> (a -> int) =
  fun w  ->
    match w with
    | WrapPoly ATag -> intA
    | WrapPoly _ -> intA (* This should not be allowed *)
;;

let _ =  example6 (WrapPoly AandBTags) `TagB (* This causes a seg fault *)
;;
