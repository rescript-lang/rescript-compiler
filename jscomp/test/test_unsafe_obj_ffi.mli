

val f : < height : int; width : int; .. > Js.t -> int


val g : 
  < method1 : (int * unit) Fn.t; method2 : (int * int * unit) Fn.t; .. > 
  Js.t -> unit

val h :
  < height__set : int -> 'a; width__set : int -> unit; .. > Js.t -> unit
