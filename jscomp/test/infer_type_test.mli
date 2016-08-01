external mk_config :
  hi:int -> lo:int -> ?width:int -> unit -> _  =
  "" [@@bs.obj]

type hh =  < hi : int; lo : int; width : int option Js.undefined > Js.t
val hh : hh
