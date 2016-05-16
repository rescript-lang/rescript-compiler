  (** 
     [%bs (req Js.t * resp Js.t => unit ) => server Js.t 
     ]

     A syntax extension

     (req Js.t ->  resp Js.t -> unit  [@uncurry] )-> server Js.t [@uncurry]
     type a = [%bs (req Js.t * resp Js.t => unit ) => server Js.t ]
  *)





type req 
type resp = <
   statusCode__set : int Js.set ;
   setHeader : string * string -> unit [@uncurry];
   end__ : string ->  unit [@uncurry]
>

type server = <
   listen : int * string *  (unit -> unit  [@uncurry]) -> unit [@uncurry]
>



type http = <
   createServer : (req Js.t * resp Js.t -> unit [@uncurry]) ->  server Js.t [@uncurry]
>


external http : http Js.t = "http"  [@@bs.val_of_module ]
