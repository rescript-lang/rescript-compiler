(* Bad (t = t) *)
module rec A : sig type t = B.t end = struct type t = B.t end
       and B : sig type t = A.t end = struct type t = A.t end;;
