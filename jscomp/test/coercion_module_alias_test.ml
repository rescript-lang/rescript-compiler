let l = Js.log
module C = Char;;

module C' : module type of Char = C;;
l @@ C'.chr 66;;

module C''' : (module type of C) = C';; (* fails *)

module C'' : (module type of Char) = C;;
l @@ C''.chr 66;;

module C3 = struct include Char end;;
l @@ C3.chr 66;;

let f x = let module M = struct module L = List end in M.L.length x;;
let g x = let module L = List in L.length (L.map succ x);;

module F(X:sig end) = Char;;
module C4 = F(struct end);;
l @@ C4.chr 66;;

module G(X:sig end) = X;; (* does not alias X *)
module M = G(struct end);;

module M' = struct
  module N = struct let x = 1 end
  module N' = N
end;;
l @@ M'.N'.x;;

module M'' : sig module N' : sig val x : int end end = M';;
l @@ M''.N'.x;;
module M2 = struct include M' end;;
module M3 : sig module N' : sig val x : int end end = struct include M' end;;
l @@ M3.N'.x;;
module M3' : sig module N' : sig val x : int end end = M2;;
l @@ M3'.N'.x;;

module M4 : sig module N' : sig val x : int end end = struct
  module N = struct let x = 1 end
  module N' = N
end;;
l @@ M4.N'.x;;

module F0(X:sig end) = struct
  module N = struct let x = 1 end
  module N' = N
end;;
module G0 : functor(X:sig end) -> sig module N' : sig val x : int end end = F0;;
module M5 = G0(struct end);;
l@@ M5.N'.x;;

module M6 = struct
  module D = struct let y = 3 end
  module N = struct let x = 1 end
  module N' = N
end;;

module M1 : sig module N : sig val x : int end module N' = N end = M6;;
l@@ M1.N'.x;;
module M7 : sig module N' : sig val x : int end end =
  (M6 : sig module N : sig val x : int end module N' = N end);;
l @@ M7.N'.x;;

open M6;;
l @@ N'.x;;

module M8 = struct
  module C = Char
  module C' = C
end;;
module M9 : sig module C : sig val chr : int -> char end module C' = C end =
  M8;;
l@@ M9.C'.chr 66;;
module M10 : sig module C' : sig val chr : int -> char end end =
  (M8 : sig module C : sig val chr : int -> char end module C' = C end);;
l @@ M10.C'.chr 66;;
