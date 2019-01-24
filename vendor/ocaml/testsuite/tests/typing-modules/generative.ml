(* Using generative functors *)

(* Without type *)
module type S = sig val x : int end;;
let v = (module struct let x = 3 end : S);;
module F() = (val v);; (* ok *)
module G (X : sig end) : S = F ();; (* ok *)
module H (X : sig end) = (val v);; (* ok *)
[%%expect{|
module type S = sig val x : int end
val v : (module S) = <module>
module F : functor () -> S
module G : functor (X : sig  end) -> S
module H : functor (X : sig  end) -> S
|}];;

(* With type *)
module type S = sig type t val x : t end;;
let v = (module struct type t = int let x = 3 end : S);;
module F() = (val v);; (* ok *)
[%%expect{|
module type S = sig type t val x : t end
val v : (module S) = <module>
module F : functor () -> S
|}];;
module G (X : sig end) : S = F ();; (* fail *)
[%%expect{|
Line _, characters 29-33:
Error: This expression creates fresh types.
       It is not allowed inside applicative functors.
|}];;
module H() = F();; (* ok *)
[%%expect{|
module H : functor () -> S
|}];;

(* Alias *)
module U = struct end;;
module M = F(struct end);; (* ok *)
[%%expect{|
module U : sig  end
module M : S
|}];;
module M = F(U);; (* fail *)
[%%expect{|
Line _, characters 11-12:
Error: This is a generative functor. It can only be applied to ()
|}];;

(* Cannot coerce between applicative and generative *)
module F1 (X : sig end) = struct end;;
module F2 : functor () -> sig end = F1;; (* fail *)
[%%expect{|
module F1 : functor (X : sig  end) -> sig  end
Line _, characters 36-38:
Error: Signature mismatch:
       Modules do not match:
         functor (X : sig  end) -> sig  end
       is not included in
         functor () -> sig  end
|}];;
module F3 () = struct end;;
module F4 : functor (X : sig end) -> sig end = F3;; (* fail *)
[%%expect{|
module F3 : functor () -> sig  end
Line _, characters 47-49:
Error: Signature mismatch:
       Modules do not match:
         functor () -> sig  end
       is not included in
         functor (X : sig  end) -> sig  end
|}];;

(* tests for shortened functor notation () *)
module X (X: sig end) (Y: sig end) = functor (Z: sig end) -> struct end;;
module Y = functor (X: sig end) (Y:sig end) -> functor (Z: sig end) ->
  struct end;;
module Z = functor (_: sig end) (_:sig end) (_: sig end) -> struct end;;
module GZ : functor (X: sig end) () (Z: sig end) -> sig end
          = functor (X: sig end) () (Z: sig end) -> struct end;;
[%%expect{|
module X : functor (X : sig  end) (Y : sig  end) (Z : sig  end) -> sig  end
module Y : functor (X : sig  end) (Y : sig  end) (Z : sig  end) -> sig  end
module Z : sig  end -> sig  end -> sig  end -> sig  end
module GZ : functor (X : sig  end) () (Z : sig  end) -> sig  end
|}];;
