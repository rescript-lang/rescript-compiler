[@@@bs.config {obj_type_auto_uncurry = true } ]


class type v = object [@uncurry]
  method hey : int * int -> int 
end 

class type v2 = object
  method hey : int * int -> int 
end 

type vv = 
  < 
    hey : int * int -> int 
  > [@bs.obj] [@uncurry]

type vv2 = 
  < 
    hey : int * int -> int 
  > [@bs.obj] 


val test_v : v Js.t -> int
val test_vv : vv -> int 
