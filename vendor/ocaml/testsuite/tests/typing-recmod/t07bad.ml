(* Bad (not regular) *)
module rec A : sig type 'a t = <m: 'a list A.t> end
             = struct type 'a t = <m: 'a list A.t> end;;
