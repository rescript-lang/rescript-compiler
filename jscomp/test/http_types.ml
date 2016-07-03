[@@@bs.config{bs_class_type = true }]
  (** 
     [%bs (req Js.t * resp Js.t => unit ) => server Js.t 
     ]

     A syntax extension

     (req Js.t ->  resp Js.t -> unit  [@fn] )-> server Js.t [@fn]
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
    method listen : int ->  string -> (unit -> unit [@fn]) -> unit
  end
type server = _server Js.t 
class type _http = 
  object 
    method createServer : (req  -> resp  -> unit [@fn]) ->  server
  end
type http = _http Js.t




external http : http  = "http"  [@@bs.val_of_module ]
