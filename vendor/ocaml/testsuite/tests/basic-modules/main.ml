(* TEST
  modules = "offset.ml pr6726.ml pr7427.ml"
*)

(* PR#6435 *)

module F (M : sig
           type t
           module Set : Set.S with type elt = t
         end) =
struct
 let test set = Printf.printf "%d\n" (M.Set.cardinal set)
end

module M = F (Offset)

let () = M.test (Offset.M.Set.singleton "42")
let v = Pr6726.Test.v

(* PR#7427 *)

let () =
  try
    let module M = Pr7427.F () in
    failwith "Test failed"
  with Assert_failure _ -> ()
