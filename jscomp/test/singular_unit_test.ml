type t = A

let f0 ( x : t) = 
  match x with 
  | A as y -> y

let f1 (x : t) = 
  match x with 
  | A -> 2     

let f3 x = 
  match x with    
  | Some (A as y) -> y
  | None -> A


let v0 = ()  