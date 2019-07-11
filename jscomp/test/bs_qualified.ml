external xx : string -> unit = "xx" [@@bs.module "x", "X"]

type param

external executeCommands : string -> param array -> unit = "executeCommands"
  [@@bs.scope "commands"] [@@bs.module "vscode"] [@@bs.splice]

external env : string Js.Dict.t = "env" [@@bs.scope "process"] [@@bs.val]

let f a b c =
  executeCommands "hi" [|a; b; c|] ;
  env

external hi : string = "hi" [@@bs.module "z"] [@@bs.scope "a0", "a1", "a2"]
external ho : string = "ho" [@@bs.val] [@@bs.scope "a0", "a1", "a2"]
external imul : int -> int -> int = "imul" [@@bs.val] [@@bs.scope "Math"]

let f2 () = (hi, ho, imul 1 2)

type buffer

external makeBuffer : int -> buffer = "Buffer" [@@bs.new] [@@bs.scope "global"]

external makeBuffer1 : int -> buffer = "Buffer"
  [@@bs.new] [@@bs.scope "global", "a0", "a1", "a2"]

external makeBuffer2 : int -> buffer = "Buffer"
  [@@bs.new] [@@bs.scope "global", "a0", "a1", "a2"] [@@bs.module "X", "ZZ"]

external makeBuffer3 : int -> buffer = "makeBuffer3"
  [@@bs.new] [@@bs.scope "global", "a0", "a1", "a2"] [@@bs.module "X", "Z"]

external max : float -> float -> float = "max" [@@bs.scope "Math"] [@@bs.val]

(* TODO: `bs.val` is not necessary, by default is good? *)

type t

external create : unit -> t = "create"
  [@@bs.scope "mat4"] [@@bs.module "gl-matrix"]

(* external scope_f : t -> int = "" [@@bs.get] [@@bs.scope "hi"]*)

external getMockFn1 : t -> int -> string = ""
  [@@bs.get_index] [@@bs.scope "a0"]

external getMockFn2 : t -> int -> string = ""
  [@@bs.get_index] [@@bs.scope "a0", "a1"]

external getMockFn3 : t -> int -> string = ""
  [@@bs.get_index] [@@bs.scope "a0", "a1", "a2"]

external setMocFn1 : t -> int -> string -> unit = ""
  [@@bs.set_index] [@@bs.scope "a0"]

external setMocFn2 : t -> int -> string -> unit = ""
  [@@bs.set_index] [@@bs.scope "a0", "a1"]

external setMocFn3 : t -> int -> string -> unit = ""
  [@@bs.set_index] [@@bs.scope "a0", "a1", "a2"]

external getX1 : t -> int = "getX1" [@@bs.get] [@@bs.scope "a0"]
external getX2 : t -> int = "getX2" [@@bs.get] [@@bs.scope "a0", "a1"]
external getX3 : t -> int = "getX3" [@@bs.get] [@@bs.scope "a0", "a1", "a2"]
external setX1 : t -> int -> unit = "setX1" [@@bs.set] [@@bs.scope "a0"]
external setX2 : t -> int -> unit = "setX2" [@@bs.set] [@@bs.scope "a0", "a1"]

external setX3 : t -> int -> unit = "setX3"
  [@@bs.set] [@@bs.scope "a0", "a1", "a2"]

external setXWeird3 : t -> int -> unit = "setXWeird3"
  [@@bs.set] [@@bs.scope "a0-hi", "a1", "a2"]

external send1 : t -> int -> unit = "send1" [@@bs.send] [@@bs.scope "a0"]
external send2 : t -> int -> unit = "send2" [@@bs.send] [@@bs.scope "a0", "a1"]
external send3 : t -> int -> unit = "send3" [@@bs.send] [@@bs.scope "a0", "a1"]
external psend1 : int -> unit = "psend1" [@@bs.send.pipe: t] [@@bs.scope "a0"]

external psend2 : int -> unit = "psend2"
  [@@bs.send.pipe: t] [@@bs.scope "a0", "a1"]

external psend3 : int -> unit = "psend3"
  [@@bs.send.pipe: t] [@@bs.scope "a0", "a1"]

let f3 x =
  ignore @@ makeBuffer 20 ;
  ignore @@ makeBuffer1 20 ;
  ignore @@ makeBuffer2 100 ;
  ignore @@ makeBuffer3 20 ;
  Js.log @@ max 1.0 2.0 ;
  (*Js.log @@ scope_f x ; *)
  Js.log @@ getMockFn1 x 0 ;
  Js.log @@ getMockFn2 x 0 ;
  Js.log @@ getMockFn3 x 0 ;
  setMocFn1 x 0 "x" ;
  setMocFn2 x 0 "x" ;
  setMocFn3 x 0 "x" ;
  Js.log @@ getX1 x ;
  Js.log @@ getX2 x ;
  Js.log @@ getX3 x ;
  setX1 x 0 ;
  setX2 x 0 ;
  setX3 x 0 ;
  setXWeird3 x 0 ;
  send1 x 0 ;
  send2 x 0 ;
  send3 x 0 ;
  x |> psend1 0 ;
  x |> psend2 0 ;
  x |> psend3 0 ;
  create ()
