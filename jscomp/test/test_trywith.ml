let f g x = 
  try g x 
  with Not_found -> 0 

[@@@warning "-21"]

let u () = 
  raise Not_found;
  let f = 3 + 3 in 
  f + f
let u1 = "bad character decimal encoding \\"
let v = "bad character decimal encoding \\%c%c%c"

(** test default branch *)
type u = A | B | C | D of int  | E of char 

let f (x : u) = 
  match x with 
  | D _ -> 1 
  | A 
  | B
  | C -> 2 

  | _ -> assert false 
