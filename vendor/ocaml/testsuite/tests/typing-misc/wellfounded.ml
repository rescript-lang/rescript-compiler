(* PR#6768 *)

type _ prod = Prod : ('a * 'y) prod;;

let f : type t. t prod -> _ = function Prod ->
  let module M =
    struct
      type d = d * d
    end
  in ()
;;
[%%expect{|
type _ prod = Prod : ('a * 'y) prod
Line _, characters 6-20:
Error: The type abbreviation d is cyclic
|}];;
