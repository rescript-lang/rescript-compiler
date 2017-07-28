(* Bad (not regular) *)
module rec A : sig type 'a t = 'a list B.t end
             = struct type 'a t = 'a list B.t end
       and B : sig type 'a t = <m: 'a array B.t> end
             = struct type 'a t = <m: 'a array B.t> end;;
