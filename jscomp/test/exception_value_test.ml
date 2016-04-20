

let f () =
  raise  Not_found



let assert_f x = 
  let ()  = assert (x > 3) in 
  3 


let hh () = 
  let v = raise Not_found in 
  v + 3 
 (* TODO: comment for line column number *)
