  (** 
     [%bs (req  * resp  => unit ) => server  
     ]

     A syntax extension

     (req  ->  resp  -> unit  [@bs] )-> server  [@bs]
     type a = [%bs (req  * resp  => unit ) => server  ]
  *)





type req 

class type _resp = 
  object 
    method statusCode : int [@@bs.set]
    method setHeader : string -> string -> unit
    method _end : string -> unit 
  end[@bs]
type resp = _resp  
class type _server = 
  object 
    method listen : int ->  string -> (unit -> unit [@bs]) -> unit
  end[@bs]
type server = _server  
class type _http = 
  object 
    method createServer : (req  -> resp  -> unit [@bs]) ->  server
  end[@bs]
type http = _http 




external http : http  = "http"  [@@bs.module ]
