(* Bad (t = t) *)
module rec A : sig type t = A.t end = struct type t = A.t end;;
