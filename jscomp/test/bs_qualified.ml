



external xx : string -> unit = "" [@@bs.module "x", "X"]

type param 

external executeCommands : string -> param array -> unit = "" 
[@@bs.scope "commands"] [@@bs.module "vscode"][@@bs.splice]

external env : string Js.Dict.t = "" [@@bs.scope "process"] [@@bs.val]

let f a b c  = 
  executeCommands "hi"  [|a;b;c|];
  env

external hi : string = "" 
[@@bs.module "z"] [@@bs.scope "a0", "a1", "a2"]
external ho : string = "" 
[@@bs.val] [@@bs.scope "a0","a1","a2"]
external imul : int -> int -> int = ""
[@@bs.val] [@@bs.scope "Math"]
let f2 ()  = 
  hi , ho, imul 1 2 


type buffer
external makeBuffer : int -> buffer = "Buffer"
[@@bs.new] [@@bs.scope "global"]

external makeBuffer1 : int -> buffer = "Buffer"
[@@bs.new] [@@bs.scope "global", "a0","a1","a2"]

external makeBuffer2 : int -> buffer = "Buffer"
[@@bs.new] [@@bs.scope "global", "a0","a1","a2"] [@@bs.module "X","ZZ"]

external makeBuffer3 : int -> buffer = ""
[@@bs.new] [@@bs.scope "global", "a0","a1","a2"] [@@bs.module "X", "Z"]

let f3 () =  
  ignore @@ makeBuffer 20;
  ignore @@ makeBuffer1 20;
  ignore @@ makeBuffer2 100;
  ignore @@ makeBuffer3 20
