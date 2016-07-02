

let f x = match x with 
  | [ _ ; _] -> 2
  | [_] -> assert false
  | [] -> raise Not_found
  | _ -> assert (1 = 2); 3 
