(* Bad (t = int * t) *)
module rec A : sig type t = int * A.t end = struct type t = int * A.t end;;
