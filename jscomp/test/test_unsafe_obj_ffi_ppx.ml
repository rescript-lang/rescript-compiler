


let  f x = 
  x##length +. x##width
let i () =  ()

let h x : unit = 
  x ## height_set 3 ;
  i @@ x ## width_set 3 

let chain x = 
  x##element##length + x##element##length


(* current error message : 
   Error: '##' is not a valid value identifier.
*)
(* let syntax_error x =  *)
(*   x ## _set_height 3 3  *)

let g x  = 
  let () = x ## method1 3  in 
  x ## method2 (3,  3 )



