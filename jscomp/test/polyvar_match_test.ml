type 'a u = A of 'a option 
let f x = 
  match x with 
  | A (Some `a) -> 0 
  | A None -> 1
  | A (Some `c) -> 2 
  | A (Some (`d _)) -> 3 
  | A (Some (`e _)) -> 4  
