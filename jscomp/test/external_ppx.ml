

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


external ff : 
    hi:int -> 
    lo:(_ [@bs.as 3]) -> 
    lo2:(_ [@bs.as {json|{hi:3 }|json}]) ->
     _ = "" [@@bs.obj]

let u = ff ~hi:2     

external f : int -> int = "f" [@@genType.import "hh"]  