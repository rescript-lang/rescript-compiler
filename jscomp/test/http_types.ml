  (** 
     [%bs (req Js.t * resp Js.t => unit ) => server Js.t 
     ]

     A syntax extension

     (req Js.t ->  resp Js.t -> unit  [@uncurry] )-> server Js.t [@uncurry]
     type a = [%bs (req Js.t * resp Js.t => unit ) => server Js.t ]
  *)





type req 

type resp = 
  [%bs.obj: <
   statusCode_set : int -> unit  ;
   setHeader : string * string -> unit ;
   end_ : string ->  unit 
  >  [@uncurry] ]

type server = 
  [%bs.obj: <
    listen : int * string *  (unit -> unit) -> unit 
  >  [@uncurry] ]



type http = 
  [%bs.obj: <
   createServer : (req  * resp  -> unit ) ->  server
  >  [@uncurry] ]


external http : http  = "http"  [@@bs.val_of_module ]
