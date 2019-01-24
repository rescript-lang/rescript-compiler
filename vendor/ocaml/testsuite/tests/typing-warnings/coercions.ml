(* comment 9644 of PR#6000 *)

fun b -> if b then format_of_string "x" else "y";;
fun b -> if b then "x" else format_of_string "y";;
fun b : (_,_,_) format -> if b then "x" else "y";;

(* PR#7135 *)

module PR7135 = struct
  module M : sig type t = private int end =  struct type t = int end
  include M

  let lift2 (f : int -> int -> int) (x : t) (y : t) =
    f (x :> int) (y :> int)
end;;

(* exemple of non-ground coercion *)

module Test1 = struct
  type t = private int
  let f x = let y = if true then x else (x:t) in (y :> int)
end;;
