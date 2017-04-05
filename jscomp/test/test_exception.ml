exception Local of int 


let f () = 
  raise (Local 3)

let g () = 
  raise Not_found

let h () = raise (Test_common.U 3 )
let x () = raise (Test_common.H)

let xx () = raise(Invalid_argument "x")   


exception Nullary

let a = Nullary