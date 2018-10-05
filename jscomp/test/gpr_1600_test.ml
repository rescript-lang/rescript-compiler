


let f : (int * int -> int [@bs]) = fun [@bs] x -> let a,b = x in a + b


let obj : < hi : (int * int -> unit [@bs.meth]) > Js.t  = object
  method hi (x : int * int) =  Js.log x 
end [@bs]
(** expect *)

class type _a = object
  method empty : unit -> unit
  method currentEvents : unit -> (string * string) array
  method push : string * string -> unit 
  method needRebuild : unit -> bool 
end [@bs]

type a = _a Js.t 

let eventObj : < currentEvents : (unit -> (string * string) array [@bs.meth]);
    empty : (unit -> unit [@bs.meth]);
    needRebuild : (unit -> bool [@bs.meth]);
    push : (string * string -> unit [@bs.meth]) >
  Js.t =  object (self)
  val events : (string * string) array = [||]
  method empty () = ()
  method push a = Array.unsafe_set self##events 0 a 
  method needRebuild () = Array.length self##events <>  0
  method currentEvents () = self##events
end [@bs]
 
let f () = (eventObj : a)
