

external mk_config :
  hi:int -> lo:int -> ?width:int -> unit -> _  =
  "" [@@bs.obj]

type hh =  < hi : int; lo : int; width : int option Js.undefined > Js.t
let hh = mk_config ~hi:30 ~lo:20 ()

(* let v = hh##widt *)
let v = hh##width


