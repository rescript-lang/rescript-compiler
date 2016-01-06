


let bool_equal x y = 
  match x, y with 
  | true, true 
  | false, false 
      -> true
  | false, true
  | true, false -> 
      false 

(* TODO: 
   There is a problem for the ffi,
   for boolean function, we might do 
   [x !==0 ]
   which return the same value whether user input [true] or [false]
   u.bool_equal(false,true) *)
let assertions () = 
  
  assert (bool_equal true true);
  assert (bool_equal false false);
  assert (not (bool_equal true false));
  assert (not (bool_equal false true));
  assert (true = true);
  assert (false = false);
  assert (not (true = false));
  assert (not (false = true))
    
