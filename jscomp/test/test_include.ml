include List
(** luckily we have module alias so global module alias is fine *)

module N = List

let v = N.length

module Make (U : Set.OrderedType) = struct
  include U

  let v = compare
end

(* var $$let=$$String; *)
module X = Make (String)
module U = Make (Test_order)

(* var X=Make([0,$$let[25]]); *)
include N

(* Missing optimization alias to a number could also be removed, especially 0 *)

module N0 = N
module N1 = N0
module N2 = N1
module N3 = N2
module N4 = N3
module N5 = N4
module N6 = N5
include N6
