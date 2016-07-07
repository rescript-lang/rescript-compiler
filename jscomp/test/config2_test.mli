[@@@bs.config {obj_type_auto_uncurry  ; (* no_export = true *) 
bs_class_type 
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
  >  [@bs]]

type vv2 = 
  [%bs.obj: < 
    hey : int -> int -> int 
  > ]


val test_v : v Js.t -> int
val test_vv : vv -> int 
