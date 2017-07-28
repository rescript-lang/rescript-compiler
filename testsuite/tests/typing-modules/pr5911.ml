module type S = sig
 type t
 val x : t
end;;

module Good (X : S with type t := unit) = struct
 let () = X.x
end;;

module type T = sig module M : S end;;

module Bad (X : T with type M.t := unit) = struct
 let () = X.M.x
end;;
