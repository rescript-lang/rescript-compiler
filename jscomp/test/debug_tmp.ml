
[@@@bs.config {
  flags = [|
  (* "-drawlambda"; *)
  (* "-dlambda";  *)
  (* "-dtypedtree"; *)
  (* "-bs-diagnose" *)
  
  |]
}]


let u x =
  match x with 
  | `a 
  | `b 
  | `d
  | `c as u -> 
    Js.log "u";
    Js.log u

  | `e 
  | `f 
  | `h as v -> 
    Js.log "v";
    Js.log v
