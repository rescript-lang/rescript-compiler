external log : 'a -> unit = "log" 
[@@bs.val] [@@bs.scope "console"]
external log2 : 'a -> 'b -> unit = "log" 
[@@bs.val] [@@bs.scope "console"]
external log3 : 'a -> 'b -> 'c -> unit = "log" 
[@@bs.val] [@@bs.scope "console"]
external log4 : 'a -> 'b -> 'c -> 'd -> unit = "log" 
[@@bs.val] [@@bs.scope "console"]
external logMany : 'a array -> unit = "log"
[@@bs.val] [@@bs.scope "console"] [@@bs.splice]

external info : 'a -> unit = "info" 
[@@bs.val] [@@bs.scope "console"]
external info2 : 'a -> 'b -> unit = "info" 
[@@bs.val] [@@bs.scope "console"]
external info3 : 'a -> 'b -> 'c -> unit = "info" 
[@@bs.val] [@@bs.scope "console"]
external info4 : 'a -> 'b -> 'c -> 'd -> unit = "info" 
[@@bs.val] [@@bs.scope "console"]
external infoMany : 'a array -> unit = "info"
[@@bs.val] [@@bs.scope "console"] [@@bs.splice]

external warn : 'a -> unit = "warn" 
[@@bs.val] [@@bs.scope "console"]
external warn2 : 'a -> 'b -> unit = "warn" 
[@@bs.val] [@@bs.scope "console"]
external warn3 : 'a -> 'b -> 'c -> unit = "warn" 
[@@bs.val] [@@bs.scope "console"]
external warn4 : 'a -> 'b -> 'c -> 'd -> unit = "warn" 
[@@bs.val] [@@bs.scope "console"]
external warnMany : 'a array -> unit = "warn"
[@@bs.val] [@@bs.scope "console"] [@@bs.splice]

external error : 'a -> unit = "error" 
[@@bs.val] [@@bs.scope "console"]
external error2 : 'a -> 'b -> unit = "error" 
[@@bs.val] [@@bs.scope "console"]
external error3 : 'a -> 'b -> 'c -> unit = "error" 
[@@bs.val] [@@bs.scope "console"]
external error4 : 'a -> 'b -> 'c -> 'd -> unit = "error" 
[@@bs.val] [@@bs.scope "console"]
external errorMany : 'a array -> unit = "error"
[@@bs.val] [@@bs.scope "console"] [@@bs.splice]

external trace : unit -> unit = "trace" [@@bs.val] [@@bs.scope "console"]

external timeStart : string -> unit = 
  "time" [@@bs.val] [@@bs.scope "console"]

external timeEnd : string -> unit = "timeEnd" 
  [@@bs.val] [@@bs.scope "console"]
