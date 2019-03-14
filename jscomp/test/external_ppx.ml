

external make_config : length:int -> width:int -> unit = "" 
    [@@bs.obj]

external make_config :  length:'a -> width:int -> unit = "" 
    [@@bs.obj]
(** Note that 
    {[ 'a . length: 'a -> width:int -> unit
    ]} is a syntax error -- check where it is allowed
*)
 


external opt_make : 
  length: int -> ?width:int -> (_ as 'event)  = 
  "" [@@bs.obj]


external f : int -> int = "" [@@gentype.import "hh"]  