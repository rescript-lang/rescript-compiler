
class type ['k,'v] arrayLike = 
  object 
    method case : 'k -> 'v Js.Null.t [@uncurry]
    method case__set : 'k * 'v -> unit [@uncurry]
    method case__unsafe : 'k -> 'v [@uncurry]
    method length : int 
  end

class type floatArray = [int, float] arrayLike

class type intArray = [int, int] arrayLike


let sum_float_array (arr : floatArray Js.t) = 
  let v = ref 0. in
  for i = 0 to arr #.length - 1 do 
    v := !v +. arr##case__unsafe i     
  done;
  !v 

let sum_int_array (arr : intArray Js.t) = 
  let v = ref 0 in

  for i = 0 to arr#.length - 1 do 
    v := !v + arr## case__unsafe i     
  done;
  !v 

let sum_poly zero add (arr : _ arrayLike Js.t) = 
  let v = ref zero in 
  for i = 0 to arr #.length - 1 do 
    v := add  !v  (arr##case__unsafe i )
  done;
  !v 
