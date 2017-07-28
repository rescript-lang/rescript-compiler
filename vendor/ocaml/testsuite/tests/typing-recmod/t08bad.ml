(* Bad (not regular) *)
module rec A : sig type 'a t = <m: 'a list B.t; n: 'a array B.t> end
             = struct type 'a t = <m: 'a list B.t; n: 'a array B.t> end
       and B : sig type 'a t = 'a A.t end = struct type 'a t = 'a A.t end;;
