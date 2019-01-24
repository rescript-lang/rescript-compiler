let flag = ref false
module F(S : sig module type T end) (A : S.T) (B : S.T) =
struct
  module X = (val if !flag then (module A) else (module B) : S.T)
end

(* If the above were accepted, one could break soundness *)
module type S = sig type t val x : t end
module Float = struct type t = float let x = 0.0 end
module Int = struct type t = int let x = 0 end

module M = F(struct module type T = S end)

let () = flag := false
module M1 = M(Float)(Int)

let () = flag := true
module M2 = M(Float)(Int)

let _ = [| M2.X.x; M1.X.x |]
