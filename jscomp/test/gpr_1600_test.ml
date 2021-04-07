


let f : (int * int -> int [@bs]) = fun [@bs] x -> let a,b = x in a + b


let obj : < hi : (int * int -> unit [@bs.meth]) >   = object
  method hi (x : int * int) =  Js.log x 
end 
(** expect *)

class type a = object
  method empty : unit -> unit
  method currentEvents : unit -> (string * string) array
  method push : string * string -> unit 
  method needRebuild : unit -> bool 
end 



let eventObj : < currentEvents : (unit -> (string * string) array [@bs.meth]);
    empty : (unit -> unit [@bs.meth]);
    needRebuild : (unit -> bool [@bs.meth]);
    push : (string * string -> unit [@bs.meth]) >
   =  object(self)
  val events : (string * string) array = [||]
  method empty () = ()
  method push a = Array.unsafe_set self##events 0 a 
  method needRebuild () = Array.length self##events <>  0
  method currentEvents () = self##events
end 
 
let f () = (eventObj : a)
