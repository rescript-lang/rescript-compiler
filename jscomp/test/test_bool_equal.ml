


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
    

let f0 x = 
  if x = true then 1 else 2 
let f1 x = 
  if not (x = true) then 1 else 2 

let f2 x =   
  if x = Js.true_ then 1 else 2  
let f3 x =   
  if x = Js.false_ then 1 else 2 

let f4 x =   
  if not (x = Js.true_) then 1 else 2 

let f5 x = 
  match x with 
  | [] -> 1 
  | _ -> 2 

  
let f6 x =   
  if x = [] then 1  
  else 2 