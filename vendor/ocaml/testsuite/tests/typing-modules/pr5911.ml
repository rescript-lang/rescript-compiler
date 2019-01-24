module type S = sig
 type t
 val x : t
end;;

module Good (X : S with type t := unit) = struct
 let () = X.x
end;;
[%%expect{|
module type S = sig type t val x : t end
module Good : functor (X : sig val x : unit end) -> sig  end
|}];;

module type T = sig module M : S end;;

module Bad (X : T with type M.t = unit) = struct
 let () = X.M.x
end;;
[%%expect{|
module type T = sig module M : S end
module Bad :
  functor (X : sig module M : sig type t = unit val x : t end end) ->
    sig  end
|}];;
