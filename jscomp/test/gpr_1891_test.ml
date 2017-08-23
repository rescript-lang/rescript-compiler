
let foo  x = 
  match x with 
  | `Foo 3 -> print_endline "1"
  | _ -> print_endline "2"


let foo2  x = 
  match x with 
  | `Foo 3 -> 
    "xxxx"
  | _ -> "xxx"

  
let foo3  x = 
  match x with 
  | `Foo 3 -> 
    1
  | _ -> 2
  

  
  