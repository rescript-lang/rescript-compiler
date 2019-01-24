(* Bad (not regular) *)
module rec A : sig type 'a t = 'a B.t end
             = struct type 'a t = 'a B.t end
       and B : sig type 'a t = <m: 'a list A.t; n: 'a array A.t> end
             = struct type 'a t = <m: 'a list A.t; n: 'a array A.t> end;;
