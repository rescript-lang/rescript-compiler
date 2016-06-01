[@@@bs.config{
  obj_type_auto_uncurry = true;
  (* non_export = true; *)
}]


class type v = object [@uncurry]
  method hi : int * int -> int 
end 


type vv = 
  < 
    hey : int * int -> int 
  > [@bs.obj]

val h : v Js.t -> int 

val test_vv : vv -> int 
