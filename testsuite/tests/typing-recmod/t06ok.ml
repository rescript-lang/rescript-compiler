(* OK (t = <m:t>) *)
module rec A : sig type t = <m:B.t> end = struct type t = <m:B.t> end
       and B : sig type t = A.t end = struct type t = A.t end;;
