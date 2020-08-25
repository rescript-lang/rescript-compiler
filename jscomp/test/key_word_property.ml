(* [@@@bs.config {flags = [|
  "-bs-package-output"; "es6:jscomp/test"
|]}] 
*)
(* FIXME it does not work*)


type t 


external default :   t = "default" [@@bs.module "some-es6-module"]
external default2 :   t = "default2" [@@bs.module "some-es6-module"]
let default,default2  = default, default2


external oefault :   t = "default" [@@bs.module "./ome-es6-module"]
external oefault2 :   t = "default2" [@@bs.module "./ome-es6-module"]
let oefault,oefault2  = oefault, oefault2


type window
external window : window = "window" [@@bs.val] [@@bs.module "vscode"]

let window = window
let mk window default = [%obj{window; default ; }]
type t_ = { window : int ; default : int }

let mk2 window default = [{window; default ; }]

let des v = [%obj{window = v##window ; default = v##default }]


let case = 3

let test =  [%obj{case ; window = 3}]

external switch : window -> string = "switch" [@@bs.send]

let u () = switch window




  (* 0,0,0,0,0,0,0,0,0,0,0,0,0,0 *)
