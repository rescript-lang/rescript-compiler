[@@@bs.config{bs_class_type = true }]
class type ['k,'v] arrayLike = 
  object 
    method case : 'k -> 'v Js.Null.t 
    method caseSet : 'k * 'v -> unit 
    method case_unsafe : 'k -> 'v 
    method length : int 
  end

class type floatArray = [int, float] arrayLike

class type intArray = [int, int] arrayLike


let sum_float_array (arr : floatArray Js.t) = 
  let v = ref 0. in
  for i = 0 to arr##length - 1 do 
    v := !v +. arr##case_unsafe i     
  done;
  !v 

let sum_int_array (arr : intArray Js.t) = 
  let v = ref 0 in
  for i = 0 to arr##length - 1 do 
    v := !v + arr## case_unsafe i     
  done;
  !v 

(* TODO: warning about unprocessed attributes *)
let sum_poly zero add (arr : _ arrayLike Js.t) = 
  let v = ref zero in 
  for i = 0 to arr##length - 1 do 
    v := add  !v  (arr##case_unsafe i ) [@fn] 
  done;
  !v 


(* TODO: create a special type 
   ['a Js.prop_set] for better error message
*)
let test_set x = 
  x##length_aux #= 3 
