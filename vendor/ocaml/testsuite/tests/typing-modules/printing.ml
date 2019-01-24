(* PR#6650 *)

module type S = sig
  class type c = object method m : int end
  module M : sig
    class type d = c
  end
end;;
module F (X : S) = X.M;;
[%%expect{|
module type S =
  sig
    class type c = object method m : int end
    module M : sig class type d = c end
  end
module F : functor (X : S) -> sig class type d = X.c end
|}];;

(* PR#6648 *)

module M = struct module N = struct let x = 1 end end;;
#show_module M;;
[%%expect{|
module M : sig module N : sig val x : int end end
module M : sig module N : sig ... end end
|}];;
