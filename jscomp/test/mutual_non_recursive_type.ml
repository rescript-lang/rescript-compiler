module U = struct
  type t = OT

  let f x = x
end

open U

type nonrec t =
  | Ta of t (* * u compilation error [nonrec applices to all] *)
  | Tb of int

and u = H of t  (** one attribute nonrecursive will affect all *)

(* refers to old t *)

let v : u = H OT
