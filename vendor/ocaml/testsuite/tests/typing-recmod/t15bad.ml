(* Bad - PR 4512 *)
module type S' = sig type t = int end
module rec M : S' with type t = M.t = struct type t = M.t end;;
