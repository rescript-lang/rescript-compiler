type (_, _) t =
 A : ('a, 'a) t
| B : string -> ('a, 'b) t
;;

module M (A : sig module type T end) (B : sig module type T end) =
struct
 let f : ((module A.T), (module B.T)) t -> string = function
   | B s -> s
end;;

module A = struct module type T = sig end end;;

module N = M(A)(A);;

let x = N.f A;;

[%%expect{|
type (_, _) t = A : ('a, 'a) t | B : string -> ('a, 'b) t
Line _, characters 52-74:
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
A
module M :
  functor (A : sig module type T end) (B : sig module type T end) ->
    sig val f : ((module A.T), (module B.T)) t -> string end
module A : sig module type T = sig  end end
module N : sig val f : ((module A.T), (module A.T)) t -> string end
Exception: Match_failure ("", 8, 52).
|}];;
