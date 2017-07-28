(* Bad (t = t -> int) *)
module rec A : sig type t = B.t -> int end = struct type t = B.t -> int end
       and B : sig type t = A.t end = struct type t = A.t end;;
