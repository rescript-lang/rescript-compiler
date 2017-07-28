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
