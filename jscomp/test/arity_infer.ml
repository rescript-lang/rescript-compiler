



let f0 x = 
  (if x > 3 then fun x -> x + 1 
  else (raise Not_found)) 3


let f1 x =   
  (raise Not_found : _ -> _ ) x 


let f3 x =   
  (match x with 
  | 0 -> fun x -> x + 1
  | 1 -> fun x -> x + 2
  | 2 -> fun  x -> x + 3
  | 3 -> fun  x -> x + 4
  | _ -> raise Not_found
  ) 3 