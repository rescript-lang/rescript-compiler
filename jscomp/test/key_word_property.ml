type t

external default : t = "default" [@@bs.module "some-es6-module"]

let default = default

type window

external window : window = "window" [@@bs.val] [@@bs.module "vscode"]

let window = window
let mk window default = [%obj {window; default}]

type t_ = {window: int; default: int}

let mk2 window default = [{window; default}]
let des v = [%obj {window= v##window; default= v##default}]
let case = 3
let test = [%obj {case; window= 3}]

external switch : window -> string = "switch" [@@bs.send]

let u () = switch window

(* 0,0,0,0,0,0,0,0,0,0,0,0,0,0 *)
