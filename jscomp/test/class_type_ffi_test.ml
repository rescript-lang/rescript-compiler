
class type ['k,'v] arrayLike = 
  object 
    method index__r : 'k -> 'v Js.opt
    method index__w : 'k -> 'v -> unit
    method index__r_unsafe : 'k -> 'v 
    method length__r : int 
  end

class type floatArray = [int, float] arrayLike

class type intArray = [int, int] arrayLike


let sum_float_array (arr : floatArray) = 
  let v = ref 0. in
  for i = 0 to arr # length__r - 1 do 
    v := !v +. arr # index__r_unsafe i     
  done;
  !v 

let sum_int_array (arr : intArray) = 
  let v = ref 0 in
  for i = 0 to arr # length__r - 1 do 
    v := !v + arr # index__r_unsafe i     
  done;
  !v 

let sum_poly zero add (arr : _ arrayLike) = 
  let v = ref zero in 
  for i = 0 to arr # length__r - 1 do 
    v := add  !v  (arr # index__r_unsafe i )
  done;
  !v 
