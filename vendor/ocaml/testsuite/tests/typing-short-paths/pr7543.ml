(** Test that short-path printtyp does not fail on packed module.

  Packed modules does not respect the arity of type constructor, which can break
  the path normalization within the short-path code path.
*)
module type S = sig type t end;;
module N = struct type 'a t = 'a end;;
let f (module M:S with type t = unit) = ();;
let () = f (module N);;
