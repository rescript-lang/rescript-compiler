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
    method setSeconds__2 :  number -> number ->  number

    method setUTCSeconds : number -> number
    method setUTCSeconds__2 : number -> number -> number

    method setMinutes : number -> number
    method setMinutes__2 : number -> number -> number
    method setMinutes__3 : number -> number -> number -> number

    method setUTCMinutes : number -> number
    method setUTCMinutes__2 : number -> number -> number
    method setUTCMinutes__3 : number -> number -> number -> number

    method setHours : number -> number
    method setHours__2 : number -> number -> number
    method setHours__3 : number -> number -> number -> number
    method setHours__4 : number -> number -> number -> number -> number

    method setUTCHours : number -> number
    method setUTCHours__2 : number -> number -> number
    method setUTCHours__3 : number -> number -> number -> number
    method setUTCHours__4 : number -> number -> number -> number -> number



    method setDate :  number ->  number
    method setUTCDate : number ->  number
    method setMonth : number -> number
    method setMonth__2 : number -> number -> number
    method setUTCMonth : number -> number
    method setUTCMonth__2 : number -> number -> number


    method setFullYear : number -> number
    method setFullYear__2 : number -> number -> number
    method setFullYear__3 : number -> number -> number -> number

    method setUTCFullYear : number -> number
    method setUTCFullYear__2 : number -> number -> number
    method setUTCFullYear__3 : number -> number -> number -> number

    method toUTCString : unit -> string
    method toISOString : unit -> string
    method toJSON__ : unit -> string
    method toJSON__1 : 'a -> string
  end[@bs]

type t = date Js.t

external current_date_as_string : unit -> string = ""
  [@@bs.val "Date"] (* [@@bs.nullary] *)
(* Note here [Date(0)] is the same as [Date()],
   but this is not always true
*)


external parse : string -> number = ""
  [@@bs.val "Date.parse"]

external now : unit -> number = ""
  [@@bs.val "Date.now"]

external current : unit -> t = "Date"
  [@@bs.new ] (* [@@bs.nullary] *)
external of_string : string -> t = "Date"
  [@@bs.new ]



external utc_of_y_m :
  year:number -> month:number -> unit -> number = ""
  [@@bs.val "Date.UTC"]

external utc_of_y_m_d :
  year:number -> month:number -> day:number ->   unit -> number = ""
  [@@bs.val "Date.UTC"]

external utc_of_y_m_d_h :
  year:number -> month:number ->
  day:number -> hour:number ->
  unit -> number = ""
  [@@bs.val "Date.UTC"]


external utc_of_y_m_d_h_m :
  year:number -> month:number ->
  day:number -> hour:number ->
  minute:number ->
  unit -> number = ""
  [@@bs.val "Date.UTC"]


external utc_of_y_m_d_h_m_s :
  year:number -> month:number ->
  day:number -> hour:number ->
  minute:number -> second:number ->
  unit -> number = ""
  [@@bs.val "Date.UTC"]

external utc_of_y_m_d_h_m_s_m :
  year:number -> month:number ->
  day:number -> hour:number  ->
  minute:number  -> second:number  ->
  millisecond:number -> unit -> t = ""
  [@@bs.val "Date.UTC"]


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


let d = of_y_m ~month:2. ~year:2016.  ()
let d2 = of_y_m_d ~month:2.
let d3 = d2 ~year:2016. ~day:1. ()


let suites = Mt.[
    "getMonth", (fun _ -> Eq(2., d##getMonth () ));
    "getYear", (fun _ -> Eq((2016.,2.,1.),
                            (d3##getFullYear() ,
                             d3##getMonth(),
                             d3##getDate()
                            )))
]

;; Mt.from_pair_suites __MODULE__ suites
