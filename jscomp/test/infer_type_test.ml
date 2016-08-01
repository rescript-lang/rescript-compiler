

external mk_config :
  hi:int -> lo:int -> ?width:int -> unit -> _  =
  "" [@@bs.obj]


let hh = mk_config ~hi:30 ~lo:20 ()

let v = hh##width
