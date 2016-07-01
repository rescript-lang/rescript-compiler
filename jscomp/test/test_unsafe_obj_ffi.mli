

val f : < height : int; width : int; .. > Js.t -> int


val g : 
  < method1 : int -> unit [@fn];
    method2 : int ->  int -> unit [@fn]; .. > 
  Js.t -> unit

val h :
  < height_set : int -> 'a [@fn]; 
    width_set : int -> unit [@fn]; .. > Js.t -> unit
