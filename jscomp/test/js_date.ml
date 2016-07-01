(* https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date *)
(* https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/Default_parameters *)
(* https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/split *)
(**
   JS API handles optional value inconsistent
   {[
      x= new Date();
      y= new Date(null);

      z = new Date(undefined);
   ]}   
   here [x] and [y] are behaving the same
   {[
     x = "ghos" ;
     x.split('');
     x.split('', undefined);
     x.split('', null);
   ]}
   here [split] and [split(undefined)] behaves the same
*)
type number = float

class type date = 
  object
    method toDateString : unit -> string [@fn]
    method toTimeString : unit -> string [@fn]
    method toLocaleString : unit -> string [@fn]
    method toLocaleDateString : unit -> string [@fn]
    method toLocaleTimeString : unit -> string [@fn]
    method valueOf : unit -> number [@fn]
    method getTime : unit ->  number [@fn]
    method getFullYear : unit ->  number [@fn]
    method getUTCFullYear : unit ->  number [@fn]
    method getMonth : unit -> number [@fn]
    method getUTCMonth : unit -> number[@fn]
    method getDate : unit -> number [@fn]
    method getUTCDate : unit -> number [@fn]
    method getDay : unit -> number [@fn]
    method getUTCDay : unit -> number [@fn]
    method getHours :  unit -> number [@fn]
    method getUTCHours :  unit -> number [@fn]
    method getMinutes :    unit -> number [@fn]
    method getUTCMinutes :  unit -> number [@fn]
    method getSeconds :  unit -> number [@fn]
    method getUTCSeconds :  unit -> number [@fn]
    method getMilliseconds : unit -> number [@fn]
    method getUTCMilliseconds : unit -> number [@fn]
    method getTimezoneOffset : unit -> number [@fn]
    method setTime  : number ->  number [@fn]

    method setMilliseconds : number ->  number [@fn]
    method setUTCMilliseconds : number ->  number [@fn]

    method setSeconds : number -> number[@fn]
    method setSeconds__2 :  number -> number ->  number[@fn]

    method setUTCSeconds : number -> number[@fn]
    method setUTCSeconds__2 : number -> number -> number[@fn]

    method setMinutes : number -> number[@fn]
    method setMinutes__2 : number -> number -> number[@fn]
    method setMinutes__3 : number -> number -> number -> number[@fn]

    method setUTCMinutes : number -> number[@fn]
    method setUTCMinutes__2 : number -> number -> number[@fn]
    method setUTCMinutes__3 : number -> number -> number -> number[@fn]

    method setHours : number -> number[@fn]
    method setHours__2 : number -> number -> number[@fn]
    method setHours__3 : number -> number -> number -> number[@fn]
    method setHours__4 : number -> number -> number -> number -> number[@fn]

    method setUTCHours : number -> number[@fn]
    method setUTCHours__2 : number -> number -> number[@fn]
    method setUTCHours__3 : number -> number -> number -> number[@fn]
    method setUTCHours__4 : number -> number -> number -> number -> number[@fn]



    method setDate :  number ->  number[@fn]
    method setUTCDate : number ->  number [@fn]
    method setMonth : number -> number[@fn]
    method setMonth__2 : number -> number -> number[@fn]
    method setUTCMonth : number -> number[@fn]
    method setUTCMonth__2 : number -> number -> number[@fn]


    method setFullYear : number -> number[@fn]
    method setFullYear__2 : number -> number -> number[@fn]
    method setFullYear__3 : number -> number -> number -> number[@fn]

    method setUTCFullYear : number -> number[@fn]
    method setUTCFullYear__2 : number -> number -> number[@fn]
    method setUTCFullYear__3 : number -> number -> number -> number[@fn]

    method toUTCString : unit -> string[@fn]
    method toISOString : unit -> string[@fn]
    method toJSON__ : unit -> string [@fn]
    method toJSON__1 : 'a -> string[@fn]
  end

type t = date Js.t 

external current_date_as_string : unit -> string = "" 
  [@@bs.call "Date"] [@@bs.nullary]
(* Note here [Date(0)] is the same as [Date()], 
   but this is not always true
*)


external parse : string -> number = ""
  [@@bs.call "Date.parse"]

external now : unit -> number = ""
  [@@bs.call "Date.now"]

external current : unit -> t = "Date"
  [@@bs.new ] [@@bs.nullary]
external of_string : string -> t = "Date"
  [@@bs.new ]



external utc_of_y_m :   
  year:number -> month:number -> unit -> number = ""
  [@@bs.call "Date.UTC"]

external utc_of_y_m_d :  
  year:number -> month:number -> day:number ->   unit -> number = ""
  [@@bs.call "Date.UTC"]

external utc_of_y_m_d_h :  
  year:number -> month:number -> 
  day:number -> hour:number ->
  unit -> number = ""
  [@@bs.call "Date.UTC"]


external utc_of_y_m_d_h_m :
  year:number -> month:number -> 
  day:number -> hour:number ->
  minute:number -> 
  unit -> number = ""
  [@@bs.call "Date.UTC"]


external utc_of_y_m_d_h_m_s :
  year:number -> month:number -> 
  day:number -> hour:number ->
  minute:number -> second:number -> 
  unit -> number = ""
  [@@bs.call "Date.UTC"]

external utc_of_y_m_d_h_m_s_m :
  year:number -> month:number -> 
  day:number -> hour:number  -> 
  minute:number  -> second:number  ->
  millisecond:number -> unit -> t = ""
  [@@bs.call "Date.UTC"]


external of_y_m:   year:number -> month:number -> unit -> t = "Date"
  [@@bs.new ]

external of_y_m_d:  year:number -> month:number -> day:number ->   unit -> t = "Date"
  [@@bs.new ]

external of_y_m_d_h:  
  year:number -> month:number -> 
  day:number -> hour:number ->
  unit -> t = "Date"
  [@@bs.new ]

external of_y_m_d_h_m :
  year:number -> month:number -> 
  day:number -> hour:number ->
  minute:number -> 
  unit -> t = "Date"
  [@@bs.new ]

external of_y_m_d_h_m_s :
  year:number -> month:number -> 
  day:number -> hour:number ->
  minute:number -> second:number -> 
  unit -> t = "Date"
  [@@bs.new ]

external of_y_m_d_h_m_s_m:
  year:number -> month:number -> 
  day:number -> hour:number  -> 
  minute:number  -> second:number  ->
  millisecond:number -> unit -> t = "Date"
  [@@bs.new ]




