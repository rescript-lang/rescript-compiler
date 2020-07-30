
type resp 
external set_okay :
     resp -> (_ [@bs.as 200] ) -> unit = "statusCode" [@@bs.set]

external set_hi :
    resp -> (_ [@bs.as "hi"] ) -> unit = 
    "hi"
[@@bs.set]    


#if 0 then
external ff_json  : hi:int -> lo:(_[@bs.as {json|null|json}]) -> _ = "" [@@bs.obj]

let uu : < hi : int; lo : string > Js.t = ff_json ~hi:3
#end

let f resp =
    set_okay resp ;
    set_hi resp 



