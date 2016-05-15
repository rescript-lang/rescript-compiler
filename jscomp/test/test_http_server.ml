


let port = 3000
let hostname = "127.0.0.1"


  (** 
     [%bs (req Js.t * resp Js.t => unit ) => server Js.t 
     ]

     A syntax extension

     (req Js.t ->  resp Js.t -> unit  [@uncurry] )-> server Js.t [@uncurry]
     type a = [%bs (req Js.t * resp Js.t => unit ) => server Js.t ]
  *)





let create_server  http = 
  let server = http##createServer (fun %uncurry  (req,  resp)  -> 
      resp##statusCode__set 200;
      resp##setHeader("Content-Type", "text/plain");
      resp##end__("Hello world\n")
    )
  in
  server##listen(port, hostname,  fun %uncurry () -> 
      Js.log ("Server running at http://"^ hostname ^ ":" ^ string_of_int port ^ "/")
    ) 



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

(*
external http : http Js.t = "http" [@@bs.val] [@@bs.module "http"]

let () = 
  create_server http
*)
