

type shape 

val init_mod : 
  (string * int * int) -> 
  shape -> 
  Obj.t

val update_mod:   
  shape -> 
  Obj.t -> 
  Obj.t -> 
  unit 