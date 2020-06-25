
[@@@bs.config {
  flags = [|
  (* "-drawlambda"; *)
  (* "-dlambda";  *)
  (* "-dtypedtree"; *)
  (* "-bs-diagnose" *)
  "-bs-g"  
  |]
}]

type t = A of int * int 

let u  = A (1,2)
let h = [1]