



class type v = object 
  method hey : int -> int -> int 
end

class type v2 = object
  method hey : int ->  int -> int 
end

type vv = 
   < 
    hey : int -> int -> int [@bs]
  > 


type vv2 = 
  < 
    hey : int ->  int -> int [@bs]
  > 


let hh (x : v) : v2 = x 

let hh2 ( x : vv) : vv2 = x 


let test_v (x : v ) = 
  x##hey 1 2

let test_vv (h : vv) =
  let hey = h##hey in hey  1 2 [@bs]
