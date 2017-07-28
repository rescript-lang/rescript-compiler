module Core = struct
  module Int = struct
    module T = struct
      type t = int
      let compare = compare
      let (+) x y = x + y
    end
    include T
    module Map = Map.Make(T)
  end

  module Std = struct
    module Int = Int
  end
end
;;

open Core.Std
;;

let x = Int.Map.empty ;;
let y = x + x ;;

(* Avoid ambiguity *)

module M = struct type t = A type u = C end
module N = struct type t = B end
open M open N;;
A;;
B;;
C;;

include M open M;;
C;;

module L = struct type v = V end
open L;;
V;;
module L = struct type v = V end
open L;;
V;;


type t1 = A;;
module M1 = struct type u = v and v = t1 end;;
module N1 = struct type u = v and v = M1.v end;;
type t1 = B;;
module N2 = struct type u = v and v = M1.v end;;


(* PR#6566 *)
module type PR6566 = sig type t = string end;;
module PR6566 = struct type t = int end;;
module PR6566' : PR6566 = PR6566;;

module A = struct module B = struct type t = T end end;;
module M2 = struct type u = A.B.t type foo = int type v = A.B.t end;;
