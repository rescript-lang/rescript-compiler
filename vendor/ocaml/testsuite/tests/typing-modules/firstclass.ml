module type S = sig type u type t end;;
module type S' = sig type t = int type u = bool end;;

(* ok to convert between structurally equal signatures, and parameters
   are inferred *)
let f (x : (module S with type t = 'a and type u = 'b)) = (x : (module S'));;
let g x = (x : (module S with type t = 'a and type u = 'b) :> (module S'));;
[%%expect{|
module type S = sig type u type t end
module type S' = sig type t = int type u = bool end
val f : (module S with type t = int and type u = bool) -> (module S') = <fun>
val g : (module S with type t = int and type u = bool) -> (module S') = <fun>
|}];;

(* with subtyping it is also ok to forget some types *)
module type S2 = sig type u type t type w end;;
let g2 x = (x : (module S2 with type t = 'a and type u = 'b) :> (module S'));;
let h x = (x : (module S2 with type t = 'a) :> (module S with type t = 'a));;
let f2 (x : (module S2 with type t = 'a and type u = 'b)) =
  (x : (module S'));; (* fail *)
let k (x : (module S2 with type t = 'a)) =
  (x : (module S with type t = 'a));; (* fail *)
[%%expect{|
module type S2 = sig type u type t type w end
val g2 : (module S2 with type t = int and type u = bool) -> (module S') =
  <fun>
val h : (module S2 with type t = 'a) -> (module S with type t = 'a) = <fun>
Line _, characters 3-4:
Error: This expression has type
         (module S2 with type t = int and type u = bool)
       but an expression was expected of type (module S')
|}];;

(* but you cannot forget values (no physical coercions) *)
module type S3 = sig type u type t val x : int end;;
let g3 x =
  (x : (module S3 with type t = 'a and type u = 'b) :> (module S'));; (* fail *)
[%%expect{|
module type S3 = sig type u type t val x : int end
Line _, characters 2-67:
Error: Type (module S3 with type t = int and type u = bool)
       is not a subtype of (module S')
|}];;
