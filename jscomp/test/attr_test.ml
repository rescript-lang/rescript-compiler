
let u = fun [@uncurry] (x,y) -> x + y 

(* let h = u (1,2) [@uncurry]  *)

type u = < v : int ; y : int > [@uncurry]

type number = float

class type date = 
  object [@uncurry]
    method toDateString : unit -> string 
    method getTime : unit ->  number 
    method setMilliseconds : number ->  number 
    method setSeconds : number -> number
    method setSeconds__2 :  number * number ->  number
    method setUTCFullYear__3 : number * number *  number -> number
    method toUTCString : unit -> string
    method toISOString : unit -> string
    method toJSON__ : unit -> string 
    method toJSON__1 : 'a -> string
  end


