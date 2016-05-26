  (** 
     [%bs (req Js.t * resp Js.t => unit ) => server Js.t 
     ]

     A syntax extension

     (req Js.t ->  resp Js.t -> unit  [@uncurry] )-> server Js.t [@uncurry]
     type a = [%bs (req Js.t * resp Js.t => unit ) => server Js.t ]
  *)





type req 

type resp = 
  <
   statusCode__set : int -> unit  ;
   setHeader : string * string -> unit ;
   end__ : string ->  unit 
  > [@bs.obj] [@uncurry]

type server = 
  <
    listen : int * string *  (unit -> unit) -> unit 
  > [@bs.obj] [@uncurry]



type http = 
  <
   createServer : (req  * resp  -> unit ) ->  server
  > [@bs.obj] [@uncurry]


external http : http  = "http"  [@@bs.val_of_module ]
