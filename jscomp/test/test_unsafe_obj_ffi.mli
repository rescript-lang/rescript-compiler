[@@@bs.config{bs_class_type = true }]

val f : < height : int; width : int; .. > Js.t -> int


val g : 
  < method1 : int -> unit [@fn];
    method2 : int ->  int -> unit [@fn]; .. > 
  Js.t -> unit
class type _metric = object method height : int [@@bs.set] method width : int [@@bs.set] end
val h : _metric Js.t -> unit
