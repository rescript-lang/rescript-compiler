

external log : 'a -> unit = ""  [@@bs.val] [@@bs.scope "console"]
external warn : 'a -> unit = "" [@@bs.val] [@@bs.scope "console"]

external timeStart : string -> unit = 
  "time" [@@bs.val] [@@bs.scope "console"]

external timeEnd : string -> unit = "timeEnd" 
  [@@bs.val] [@@bs.scope "console"]
