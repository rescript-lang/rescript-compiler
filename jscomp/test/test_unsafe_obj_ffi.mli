

val f : < height : int; width : int; .. >  -> int


val g : 
  < method1 : int -> unit [@bs];
    method2 : int ->  int -> unit [@bs]; .. > 
  -> unit
class type _metric = object method height : int [@@bs.set] method width : int [@@bs.set] end
val h : _metric  -> unit
