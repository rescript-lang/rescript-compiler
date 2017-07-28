module C = Char;;
C.chr 66;;

module C' : module type of Char = C;;
C'.chr 66;;

module C3 = struct include Char end;;
C3.chr 66;;

let f x = let module M = struct module L = List end in M.L.length x;;
let g x = let module L = List in L.length (L.map succ x);;

module F(X:sig end) = Char;;
module C4 = F(struct end);;
C4.chr 66;;

module G(X:sig end) = struct module M = X end;; (* does not alias X *)
module M = G(struct end);;

module M' = struct
  module N = struct let x = 1 end
  module N' = N
end;;
M'.N'.x;;

module M'' : sig module N' : sig val x : int end end = M';;
M''.N'.x;;
module M2 = struct include M' end;;
module M3 : sig module N' : sig val x : int end end = struct include M' end;;
M3.N'.x;;
module M3' : sig module N' : sig val x : int end end = M2;;
M3'.N'.x;;

module M4 : sig module N' : sig val x : int end end = struct
  module N = struct let x = 1 end
  module N' = N
end;;
M4.N'.x;;

module F(X:sig end) = struct
  module N = struct let x = 1 end
  module N' = N
end;;
module G : functor(X:sig end) -> sig module N' : sig val x : int end end = F;;
module M5 = G(struct end);;
M5.N'.x;;

module M = struct
  module D = struct let y = 3 end
  module N = struct let x = 1 end
  module N' = N
end;;

module M1 : sig module N : sig val x : int end module N' = N end = M;;
M1.N'.x;;
module M2 : sig module N' : sig val x : int end end =
  (M : sig module N : sig val x : int end module N' = N end);;
M2.N'.x;;

open M;;
N'.x;;

module M = struct
  module C = Char
  module C' = C
end;;
module M1
  : sig module C : sig val escaped : char -> string end module C' = C end
  = M;; (* sound, but should probably fail *)
M1.C'.escaped 'A';;
module M2 : sig module C' : sig val chr : int -> char end end =
  (M : sig module C : sig val chr : int -> char end module C' = C end);;
M2.C'.chr 66;;

StdLabels.List.map;;

module Q = Queue;;
exception QE = Q.Empty;;
try Q.pop (Q.create ()) with QE -> "Ok";;

module type Complex = module type of Complex with type t = Complex.t;;
module M : sig module C : Complex end = struct module C = Complex end;;

module C = Complex;;
C.one.Complex.re;;
include C;;

module F(X:sig module C = Char end) = struct module C = X.C end;;

(* Applicative functors *)
module S = String
module StringSet = Set.Make(String)
module SSet = Set.Make(S);;
let f (x : StringSet.t) = (x : SSet.t);;

(* Also using include (cf. Leo's mail 2013-11-16) *)
module F (M : sig end) : sig type t end = struct type t = int end
module T = struct
  module M = struct end
  include F(M)
end;;
include T;;
let f (x : t) : T.t = x ;;

(* PR#4049 *)
(* This works thanks to abbreviations *)
module A = struct
  module B = struct type t let compare x y = 0 end
  module S = Set.Make(B)
  let empty = S.empty
end
module A1 = A;;
A1.empty = A.empty;;

(* PR#3476 *)
(* Does not work yet *)
module FF(X : sig end) = struct type t end
module M = struct
  module X = struct end
  module Y = FF (X) (* XXX *)
  type t = Y.t
end
module F (Y : sig type t end) (M : sig type t = Y.t end) = struct end;;

module G = F (M.Y);;
(*module N = G (M);;
module N = F (M.Y) (M);;*)

(* PR#6307 *)

module A1 = struct end
module A2 = struct end
module L1 = struct module X = A1 end
module L2 = struct module X = A2 end;;

module F (L : (module type of L1)) = struct end;;

module F1 = F(L1);; (* ok *)
module F2 = F(L2);; (* should succeed too *)

(* Counter example: why we need to be careful with PR#6307 *)
module Int = struct type t = int let compare = compare end
module SInt = Set.Make(Int)
type (_,_) eq = Eq : ('a,'a) eq
type wrap = W of (SInt.t, SInt.t) eq

module M = struct
  module I = Int
  type wrap' = wrap = W of (Set.Make(Int).t, Set.Make(I).t) eq
end;;
module type S = module type of M;; (* keep alias *)

module Int2 = struct type t = int let compare x y = compare y x end;;
module type S' = sig
  module I = Int2
  include S with module I := I
end;; (* fail *)

(* (* if the above succeeded, one could break invariants *)
module rec M2 : S' = M2;; (* should succeed! (but this is bad) *)

let M2.W eq = W Eq;;

let s = List.fold_right SInt.add [1;2;3] SInt.empty;;
module SInt2 = Set.Make(Int2);;
let conv : type a b. (a,b) eq -> a -> b = fun Eq x -> x;;
let s' : SInt2.t = conv eq s;;
SInt2.elements s';;
SInt2.mem 2 s';; (* invariants are broken *)
*)

(* Check behavior with submodules *)
module M = struct
  module N = struct module I = Int end
  module P = struct module I = N.I end
  module Q = struct
    type wrap' = wrap = W of (Set.Make(Int).t, Set.Make(P.I).t) eq
  end
end;;
module type S = module type of M ;;

module M = struct
  module N = struct module I = Int end
  module P = struct module I = N.I end
  module Q = struct
    type wrap' = wrap = W of (Set.Make(Int).t, Set.Make(N.I).t) eq
  end
end;;
module type S = module type of M ;;

(* PR#6365 *)
module type S = sig module M : sig type t val x : t end end;;
module H = struct type t = A let x = A end;;
module H' = H;;
module type S' = S with module M = H';; (* shouldn't introduce an alias *)

(* PR#6376 *)
module type Alias = sig module N : sig end module M = N end;;
module F (X : sig end) = struct type t end;;
module type A = Alias with module N := F(List);;
module rec Bad : A = Bad;;

(* Shinwell 2014-04-23 *)
module B = struct
 module R = struct
   type t = string
 end

 module O = R
end

module K = struct
 module E = B
 module N = E.O
end;;

let x : K.N.t = "foo";;

(* PR#6465 *)

module M = struct type t = A module B = struct type u = B end end;;
module P : sig type t = M.t = A module B = M.B end = M;; (* should be ok *)
module P : sig type t = M.t = A module B = M.B end = struct include M end;;

module type S = sig
  module M : sig module P : sig end end
  module Q = M
end;;
module type S = sig
  module M : sig module N : sig end module P : sig end end
  module Q : sig module N = M.N module P = M.P end
end;;
module R = struct
  module M = struct module N = struct end module P = struct end end
  module Q = M
end;;
module R' : S = R;; (* should be ok *)

(* PR#6578 *)

module M = struct let f x = x end
module rec R : sig module M : sig val f : 'a -> 'a end end =
  struct module M = M end;;
R.M.f 3;;
module rec R : sig module M = M end = struct module M = M end;;
R.M.f 3;;
