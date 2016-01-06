(** luckily we have module alias 
    so global module alias is fine
*)
include List 
module N = List
let v = N.length


module Make(U:Set.OrderedType) = struct
  include U
  let v = compare
end

(* var $$let=$$String; *)
module X = Make(String)
module U = Make(Test_order)
(* var X=Make([0,$$let[25]]); *)
include N


