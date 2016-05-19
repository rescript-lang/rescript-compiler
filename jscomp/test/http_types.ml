  (** 
     [%bs (req Js.t * resp Js.t => unit ) => server Js.t 
     ]

     A syntax extension

     (req Js.t ->  resp Js.t -> unit  [@uncurry] )-> server Js.t [@uncurry]
     type a = [%bs (req Js.t * resp Js.t => unit ) => server Js.t ]
  *)





type req 

type resp = [%uncurry: <
   statusCode__set : int -> unit  ;
   setHeader : string * string -> unit ;
   end__ : string ->  unit 
> Js.t ]

type server = [%uncurry: <
   listen : int * string *  (unit -> unit) -> unit 
> Js.t]



type http = [%uncurry:<
   createServer : (req  * resp  -> unit ) ->  server
> Js.t ]


external http : http  = "http"  [@@bs.val_of_module ]
