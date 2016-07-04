[@@@bs.config{bs_class_type  }]
  (** 
     [%bs (req Js.t * resp Js.t => unit ) => server Js.t 
     ]

     A syntax extension

     (req Js.t ->  resp Js.t -> unit  [@bs] )-> server Js.t [@bs]
     type a = [%bs (req Js.t * resp Js.t => unit ) => server Js.t ]
  *)





type req 

class type _resp = 
  object 
    method statusCode : int [@@bs.set]
    method setHeader : string -> string -> unit
    method end_ : string -> unit 
  end
type resp = _resp Js.t 
class type _server = 
  object 
    method listen : int ->  string -> (unit -> unit [@bs]) -> unit
  end
type server = _server Js.t 
class type _http = 
  object 
    method createServer : (req  -> resp  -> unit [@bs]) ->  server
  end
type http = _http Js.t




external http : http  = "http"  [@@bs.val_of_module ]
