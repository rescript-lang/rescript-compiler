external mk_config :
  hi:int -> lo:int -> ?width:int -> unit -> _  =
  "" [@@bs.obj]

type hh =  < hi : int; lo : int; width : int  Js.undefined > 
val hh : hh

val v : < hi : int; lo : int; width : int Js.undefined > 

val vv : < hi : int; lo : int; width : int Js.undefined > 

val u : int
val uu : int Js.undefined  
