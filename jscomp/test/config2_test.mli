[@@@bs.config {obj_type_auto_uncurry = true ; (* non_export = true *) 
bs_class_type = true
} ]


class type v = object
  method hey : int -> int -> int 
end 

class type v2 = object
  method hey : int -> int -> int 
end 

type vv = 
  [%bs.obj: < 
    hey : int -> int -> int 
  >  [@fn]]

type vv2 = 
  [%bs.obj: < 
    hey : int -> int -> int 
  > ]


val test_v : v Js.t -> int
val test_vv : vv -> int 
