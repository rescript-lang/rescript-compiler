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
  > [@bs.obj] [@uncurry]



let h ( x : v Js.t)=
  x##hi (1,2) 


let test_vv (h : vv) = 
  h##hey(1,2)
