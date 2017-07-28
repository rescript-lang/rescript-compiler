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
