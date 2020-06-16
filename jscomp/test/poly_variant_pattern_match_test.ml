
[@@@bs.config {
  flags = [|
  "-drawlambda";
  (* "-dlambda";  *)
  (* "-dtypedtree"; *)
  "-bs-diagnose"  
  |]
}]


let f x =
  match x with   
  | `a -> 1 
  | `b 
  | `c -> 2 
  | (`d _ | `e _) as f  -> 
    begin match f with 
      | `d x -> x 
      | _ -> 0
    end 


let parse_chan d =
  match  d with
  | `End -> `Error "unexpected end of file"
  | (`Ok _ | `Error _) as res -> res


let gpr_6359 u = 
  match u with 
  | `A(n,g)
  | `D(n,g) -> n + g
  | _ -> 0    

let gpr_6359_1 u = 
  match u with 
  | `A(n,g)
  | `D(n,g) -> int_of_string ("1"^n) + g
  | _ -> 0    
