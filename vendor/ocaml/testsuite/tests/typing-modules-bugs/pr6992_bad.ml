(* PR#6992, reported by Stephen Dolan *)

type (_, _) eq = Eq : ('a, 'a) eq
let cast : type a b . (a, b) eq -> a -> b = fun Eq x -> x

module Fix (F : sig type 'a f end) = struct
  type 'a fix = ('a, 'a F.f) eq
  let uniq (type a) (type b) (Eq : a fix) (Eq : b fix) : (a, b) eq = Eq
end

(* This would allow:
module FixId = Fix (struct type 'a f = 'a end)
 let bad : (int, string) eq = FixId.uniq Eq Eq
 let _ = Printf.printf "Oh dear: %s" (cast bad 42)
*)
