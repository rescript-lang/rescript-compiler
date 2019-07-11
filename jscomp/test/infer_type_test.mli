external mk_config : hi:int -> lo:int -> ?width:int -> unit -> _ = ""
  [@@bs.obj]

type hh = < hi: int ; lo: int ; width: int Js.undefined > Js.t

val hh : hh
val v : < hi: int ; lo: int ; width: int Js.undefined > Js.t
val vv : < hi: int ; lo: int ; width: int Js.undefined > Js.t
val u : int
val uu : int Js.undefined
