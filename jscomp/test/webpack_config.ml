module type Config = sig
  val configx : Js.Json.t
end
module WebpackConfig: Config = struct
  external configx : Js.Json.t = "../../../webpack.config.js" [@@bs.module]
end

module WebpackDevMiddlewareConfig: Config = struct
  external configx : Js.Json.t = "../../../webpack.middleware.config.js" [@@bs.module]
end

external configX : unit ->  Js.Json.t = ""
[@@bs.module  "../../../webpack.middleware.config.js"]
[@@bs.val]

let configX = configX
module U  : sig 
  val configX : unit -> Js.Json.t 
end = struct 
    external configX : unit -> Js.Json.t =  ""
    [@@bs.module "../../../webpack.config.js" ]
    [@@bs.val]
end
 external hey : unit -> unit  = "xx" [@@bs.module "List"] 
module A = struct 
  external ff : unit -> unit = "" [@@bs.module "reactX", "List" ]
  external ff2 : unit -> unit = "" [@@bs.module "reactX", "List" ]
end
module B = struct 
  external ff : unit -> unit = "" [@@bs.module "reactV", "List"]
  external ff2 : unit -> unit = "" [@@bs.module "reactV", "List"]
end

let f ()   = A.ff , A.ff2,  B.ff, B.ff2
 ;; hey () 

 ;; List.length [1;2] , List.length []

type t 
external ff : unit -> t = "" [@@bs.module "./local"]