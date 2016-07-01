  (** 
     [%bs (req Js.t * resp Js.t => unit ) => server Js.t 
     ]

     A syntax extension

     (req Js.t ->  resp Js.t -> unit  [@fn] )-> server Js.t [@fn]
     type a = [%bs (req Js.t * resp Js.t => unit ) => server Js.t ]
  *)





type req 

type resp = 
  [%bs.obj: <
   statusCode_set : int -> unit  ;
   setHeader : string -> string -> unit ;
   end_ : string ->  unit 
  >  [@fn] ]

type server = 
  [%bs.obj: <
    listen : int ->  string -> (unit -> unit) -> unit 
  >  [@fn] ]



type http = 
  [%bs.obj: <
   createServer : (req  -> resp  -> unit ) ->  server
  >  [@fn] ]


external http : http  = "http"  [@@bs.val_of_module ]
