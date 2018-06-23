
type 'a u = 'a option = 
  private 
   | None
   | Some  of 'a

let f0 x =
   match x with 
  | (_, (Some true)) -> 1 
  | (_, _ ) -> 2 


type x = A of int * int | None

type x0 = Some of int | None
let f1 u = match u with | A _ -> 0 | None -> 1



let f2 ?x ?(y : int option) ?(z = 3) 
  ()
   = 
   Js.log x ; 
   match y with 
   | None -> 0 
   | Some y ->  
    y + z 


let f3 x  =    
  match x with 
  | None -> 0 
  | Some _ -> 1 

let f4 x =   
  match x with 
  | None -> 0 
  | Some x -> x + 1

  
let f5 a  =   
  Some a = None

let f6 a =   
  Some a <> None