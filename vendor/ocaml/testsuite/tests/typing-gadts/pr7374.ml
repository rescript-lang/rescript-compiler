type ('a, 'b) eq = Refl : ('a, 'a) eq

module type S = sig
  type 'a t constraint 'a = [`Rec of 'b]
end;;
[%%expect{|
type ('a, 'b) eq = Refl : ('a, 'a) eq
module type S = sig type 'a t constraint 'a = [ `Rec of 'b ] end
|}]

module Fix (X : S) : sig
  type t
  val uniq : ('a, [`Rec of 'a] X.t) eq -> ('a, t) eq
end = struct
  type t = [`Rec of 'a] X.t as 'a
  let uniq : type a . (a, [`Rec of a] X.t) eq -> (a, t) eq =
    fun Refl -> Refl
end;; (* should fail *)
[%%expect{|
Line _, characters 16-20:
Error: This expression has type (a, a) eq
       but an expression was expected of type (a, t) eq
       Type a is not compatible with type t = [ `Rec of 'a ] X.t as 'a
|}]

(* trigger segfault
module Id = struct
  type 'a t = 'b constraint 'a = [ `Rec of 'b ]
end

module Bad = Fix(Id)

let segfault () =
  print_endline (cast (trans (Bad.uniq Refl) (Bad.uniq Refl)) 0)
*)

(* addendum: ensure that hidden paths are checked too *)
module F (X : sig type 'a t end) = struct
  open X
  let f : type a b. (a, b t) eq -> (b, a t) eq -> (a, a t t) eq =
    fun Refl Refl -> Refl;;
end;; (* should fail *)
[%%expect{|
Line _, characters 21-25:
Error: This expression has type (a, a) eq
       but an expression was expected of type (a, a X.t X.t) eq
       Type a = b X.t is not compatible with type a X.t X.t
       Type b is not compatible with type a X.t
|}]
