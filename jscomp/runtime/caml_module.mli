

type shape 

val init_mod :  (* [@@dead "init_mod"] *)
  (string * int * int) -> 
  shape -> 
  Caml_obj_extern.t

val update_mod:    (* [@@dead "update_mod"] *)
  shape -> 
  Caml_obj_extern.t -> 
  Caml_obj_extern.t -> 
  unit 