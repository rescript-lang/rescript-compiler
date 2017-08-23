
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
  

  
let foo4 x h = 
  match x with 
  | `Foo 3 ->  h ()
  | _ -> ()

let foo5 x = 
  match x with
  | `Foo 3 -> Js.log "hi"
  | _ -> Js.log "x"
