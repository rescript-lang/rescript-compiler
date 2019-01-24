(* PR#7324 *)

module rec T : sig type t = T.t end = T;;
[%%expect{|
Line _, characters 15-35:
Error: The type abbreviation T.t is cyclic
|}]
