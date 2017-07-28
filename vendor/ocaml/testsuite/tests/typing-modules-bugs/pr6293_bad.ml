module type S = sig type t = { a : int; b : int; } end;;
let f (module M : S with type t = int) = { M.a = 0 };;
