(* Tests for nested equations (bind abstract types from other modules) *)

type _ t = Int : int t;;

let to_int (type a) (w : a t) (x : a) : int = let Int = w in x;;
[%%expect{|
type _ t = Int : int t
val to_int : 'a t -> 'a -> int = <fun>
|}];;

let w_bool : bool t = Obj.magic 0;;
let f_bool (x : bool) : int = let Int = w_bool in x;; (* fail *)
[%%expect{|
val w_bool : bool t = Int
Line _, characters 34-37:
Error: This pattern matches values of type int t
       but a pattern was expected which matches values of type bool t
       Type int is not compatible with type bool
|}];;

let w_buffer : Buffer.t t = Obj.magic 0;;
let f_buffer (x : Buffer.t) : int = let Int = w_buffer in x;; (* ok *)
[%%expect{|
val w_buffer : Buffer.t t = Int
val f_buffer : Buffer.t -> int = <fun>
|}];;

let w_spec : Arg.spec t = Obj.magic 0;;
let f_spec (x : Arg.spec) : int = let Int = w_spec in x;; (* fail *)
[%%expect{|
val w_spec : Arg.spec t = Int
Line _, characters 38-41:
Error: This pattern matches values of type int t
       but a pattern was expected which matches values of type Arg.spec t
       Type int is not compatible with type Arg.spec
|}];;

module M : sig type u val w : u t val x : u end =
  struct type u = int let w = Int let x = 33 end;;
let m_x : int = let Int = M.w in M.x;;
[%%expect{|
module M : sig type u val w : u t val x : u end
val m_x : int = 33
|}];;

module F (X : sig type u = int val x : u end) = struct let x : int = X.x end;;
let fm_x : int = let Int = M.w in let module FM = F(M) in FM.x;; (* ok *)
[%%expect{|
module F :
  functor (X : sig type u = int val x : u end) -> sig val x : int end
val fm_x : int = 33
|}];;

module M' = struct module M : sig type u val w : u t val x : u end = M end;;
module F' (X : sig module M : sig type u = int val x : u end end) =
  struct let x : int = X.M.x end;;
let fm'_x : int =
  let Int = M'.M.w in let module FM' = F'(M') in FM'.x;; (* ok *)
[%%expect{|
module M' : sig module M : sig type u val w : u t val x : u end end
module F' :
  functor (X : sig module M : sig type u = int val x : u end end) ->
    sig val x : int end
val fm'_x : int = 33
|}];;

(* PR#7233 *)

type (_, _) eq = Refl : ('a, 'a) eq

module type S = sig
  type t
  val eql : (t, int) eq
end

module F (M : S) = struct
  let zero : M.t =
    let Refl = M.eql in 0
end;;
[%%expect{|
type (_, _) eq = Refl : ('a, 'a) eq
module type S = sig type t val eql : (t, int) eq end
module F : functor (M : S) -> sig val zero : M.t end
|}];;
