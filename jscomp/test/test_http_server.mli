
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

val create_server : http Js.t -> unit
