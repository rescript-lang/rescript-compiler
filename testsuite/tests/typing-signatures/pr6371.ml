module M = struct
  type t = int * (< m : 'a > as 'a)
end;;

module type S =
    sig module M : sig type t end end with module M = M
;;
