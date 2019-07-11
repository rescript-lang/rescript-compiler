val f : < height: int ; width: int ; .. > Js.t -> int

val g :
     < method1: int -> unit [@bs] ; method2: int -> int -> unit [@bs] ; .. >
     Js.t
  -> unit

class type _metric =
  object
    method height : int [@@bs.set]

    method width : int [@@bs.set]
  end[@bs]

val h : _metric Js.t -> unit
