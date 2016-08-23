
type t

external mk : int -> t = "xx/foo_class" [@@bs.new] [@@bs.module]

let f () =
  mk 3  

(*
TODO: more error checking
1. [@@bs.module] can only be used once
2. here [bs.new] should not have any payload
3. consolidate all [bs.module] 
external mk2 : int -> t = "xx/foo_class" [@@bs.new "x"] [@@bs.module]

let ff () =
  mk2 3  
*)

external ff : int -> t = "" [@@bs.module "xx/foo_class"]

let v () =
  ff 3  
