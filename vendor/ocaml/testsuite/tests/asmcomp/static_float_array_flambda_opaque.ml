external is_in_static_data : 'a -> bool = "caml_is_in_static_data"

let a = [|0.; 1.|]
let f = 1.23
let b = [|0.; f; f|]
let g = Sys.opaque_identity 1.23
let c = [|0.; g|]
let d = [|0.; Simple_float_const_opaque.f|]

let () = assert(is_in_static_data a)
let () = assert(is_in_static_data f)
let () = assert(is_in_static_data b)

let () = assert(not (is_in_static_data c))
(* In fact this one could be static by preallocating the array then
   patching it when g is available *)

let () = assert(not (is_in_static_data d))
(* The dependency Simple_float_const_opaque is built with opaque,
   hence the value of Simple_float_const_opaque.f cannot be known
   preventing the static allocation of d *)
