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
    method toDateString : unit -> string [@uncurry]
    method toTimeString : unit -> string [@uncurry]
    method toLocaleString : unit -> string [@uncurry]
    method toLocaleDateString : unit -> string [@uncurry]
    method toLocaleTimeString : unit -> string [@uncurry]
    method valueOf : unit -> number [@uncurry]
    method getTime : unit ->  number [@uncurry]
    method getFullYear : unit ->  number [@uncurry]
    method getUTCFullYear : unit ->  number [@uncurry]
    method getMonth : unit -> number [@uncurry]
    method getUTCMonth : unit -> number[@uncurry]
    method getDate : unit -> number [@uncurry]
    method getUTCDate : unit -> number [@uncurry]
    method getDay : unit -> number [@uncurry]
    method getUTCDay : unit -> number [@uncurry]
    method getHours :  unit -> number [@uncurry]
    method getUTCHours :  unit -> number [@uncurry]
    method getMinutes :    unit -> number [@uncurry]
    method getUTCMinutes :  unit -> number [@uncurry]
    method getSeconds :  unit -> number [@uncurry]
    method getUTCSeconds :  unit -> number [@uncurry]
    method getMilliseconds : unit -> number [@uncurry]
    method getUTCMilliseconds : unit -> number [@uncurry]
    method getTimezoneOffset : unit -> number [@uncurry]
    method setTime  : number ->  number [@uncurry]

    method setMilliseconds : number ->  number [@uncurry]
    method setUTCMilliseconds : number ->  number [@uncurry]

    method setSeconds : number -> number[@uncurry]
    method setSeconds__2 :  number -> number ->  number[@uncurry]

    method setUTCSeconds : number -> number[@uncurry]
    method setUTCSeconds__2 : number -> number -> number[@uncurry]

    method setMinutes : number -> number[@uncurry]
    method setMinutes__2 : number -> number -> number[@uncurry]
    method setMinutes__3 : number -> number -> number -> number[@uncurry]

    method setUTCMinutes : number -> number[@uncurry]
    method setUTCMinutes__2 : number -> number -> number[@uncurry]
    method setUTCMinutes__3 : number -> number -> number -> number[@uncurry]

    method setHours : number -> number[@uncurry]
    method setHours__2 : number -> number -> number[@uncurry]
    method setHours__3 : number -> number -> number -> number[@uncurry]
    method setHours__4 : number -> number -> number -> number -> number[@uncurry]

    method setUTCHours : number -> number[@uncurry]
    method setUTCHours__2 : number -> number -> number[@uncurry]
    method setUTCHours__3 : number -> number -> number -> number[@uncurry]
    method setUTCHours__4 : number -> number -> number -> number -> number[@uncurry]



    method setDate :  number ->  number[@uncurry]
    method setUTCDate : number ->  number [@uncurry]
    method setMonth : number -> number[@uncurry]
    method setMonth__2 : number -> number -> number[@uncurry]
    method setUTCMonth : number -> number[@uncurry]
    method setUTCMonth__2 : number -> number -> number[@uncurry]


    method setFullYear : number -> number[@uncurry]
    method setFullYear__2 : number -> number -> number[@uncurry]
    method setFullYear__3 : number -> number -> number -> number[@uncurry]

    method setUTCFullYear : number -> number[@uncurry]
    method setUTCFullYear__2 : number -> number -> number[@uncurry]
    method setUTCFullYear__3 : number -> number -> number -> number[@uncurry]

    method toUTCString : unit -> string[@uncurry]
    method toISOString : unit -> string[@uncurry]
    method toJSON__ : unit -> string [@uncurry]
    method toJSON__1 : 'a -> string[@uncurry]
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




