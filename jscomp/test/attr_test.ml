
let u = fun [@uncurry] (x,y) -> x + y 

let h = u (1,2) [@uncurry] 

type u = < v : int ; y : int > [@uncurry]

type number = float

class type date = 
  object [@uncurry]
    method toDateString : unit -> string 
    method toTimeString : unit -> string 
    method toLocaleString : unit -> string 
    method toLocaleDateString : unit -> string 
    method toLocaleTimeString : unit -> string 
    method valueOf : unit -> number 
    method getTime : unit ->  number 
    method getFullYear : unit ->  number 
    method getUTCFullYear : unit ->  number 
    method getMonth : unit -> number 
    method getUTCMonth : unit -> number
    method getDate : unit -> number 
    method getUTCDate : unit -> number 
    method getDay : unit -> number 
    method getUTCDay : unit -> number 
    method getHours :  unit -> number 
    method getUTCHours :  unit -> number 
    method getMinutes :    unit -> number 
    method getUTCMinutes :  unit -> number 
    method getSeconds :  unit -> number 
    method getUTCSeconds :  unit -> number 
    method getMilliseconds : unit -> number 
    method getUTCMilliseconds : unit -> number 
    method getTimezoneOffset : unit -> number 
    method setTime  : number ->  number 

    method setMilliseconds : number ->  number 
    method setUTCMilliseconds : number ->  number 

    method setSeconds : number -> number
    method setSeconds__2 :  number * number ->  number

    method setUTCSeconds : number -> number
    method setUTCSeconds__2 : number * number -> number

    method setMinutes : number -> number
    method setMinutes__2 : number * number -> number
    method setMinutes__3 : number * number * number -> number

    method setUTCMinutes : number -> number
    method setUTCMinutes__2 : number * number -> number
    method setUTCMinutes__3 : number * number * number -> number

    method setHours : number -> number
    method setHours__2 : number * number -> number
    method setHours__3 : number * number * number -> number
    method setHours__4 : number * number * number * number -> number

    method setUTCHours : number -> number
    method setUTCHours__2 : number * number -> number
    method setUTCHours__3 : number * number * number -> number
    method setUTCHours__4 : number * number * number *  number -> number



    method setDate :  number ->  number
    method setUTCDate : number ->  number 
    method setMonth : number -> number
    method setMonth__2 : number * number -> number
    method setUTCMonth : number * number
    method setUTCMonth__2 : number * number -> number


    method setFullYear : number -> number
    method setFullYear__2 : number *  number -> number
    method setFullYear__3 : number * number * number -> number

    method setUTCFullYear : number -> number
    method setUTCFullYear__2 : number * number -> number
    method setUTCFullYear__3 : number * number *  number -> number

    method toUTCString : unit -> string
    method toISOString : unit -> string
    method toJSON__ : unit -> string 
    method toJSON__1 : 'a -> string
  end
