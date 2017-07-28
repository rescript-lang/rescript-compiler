(* Using generative functors *)

(* Without type *)
module type S = sig val x : int end;;
let v = (module struct let x = 3 end : S);;
module F() = (val v);; (* ok *)
module G (X : sig end) : S = F ();; (* ok *)
module H (X : sig end) = (val v);; (* ok *)

(* With type *)
module type S = sig type t val x : t end;;
let v = (module struct type t = int let x = 3 end : S);;
module F() = (val v);; (* ok *)
module G (X : sig end) : S = F ();; (* fail *)
module H() = F();; (* ok *)

(* Alias *)
module U = struct end;;
module M = F(struct end);; (* ok *)
module M = F(U);; (* fail *)

(* Cannot coerce between applicative and generative *)
module F1 (X : sig end) = struct end;;
module F2 : functor () -> sig end = F1;; (* fail *)
module F3 () = struct end;;
module F4 : functor (X : sig end) -> sig end = F3;; (* fail *)

(* tests for shortened functor notation () *)
module X (X: sig end) (Y: sig end) = functor (Z: sig end) -> struct end;;
module Y = functor (X: sig end) (Y:sig end) -> functor (Z: sig end) ->
  struct end;;
module Z = functor (_: sig end) (_:sig end) (_: sig end) -> struct end;;
module GZ : functor (X: sig end) () (Z: sig end) -> sig end
          = functor (X: sig end) () (Z: sig end) -> struct end;;
