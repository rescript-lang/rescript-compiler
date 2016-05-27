

val f : < height : int; width : int; .. > Js.t -> int


val g : 
  < method1 : int -> unit [@uncurry];
    method2 : int * int -> unit [@uncurry]; .. > 
  Js.t -> unit

val h :
  < height_set : int -> 'a [@uncurry]; width_set : int -> unit [@uncurry]; .. > Js.t -> unit
