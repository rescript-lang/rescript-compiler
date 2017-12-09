

external min_int : int -> int -> int = "min" [@@bs.val] [@@bs.scope "Math"]

(* ATTENTION: only built-in runtime would simplify it
   as
   {[
     var min_int = Math.min
   ]}
   otherwise it has to be expanded as
   {[
     var min_int = funciton(x,y){
       return Math.min(x,y)
     }
   ]}
   There are other things like [@bs.send] which does not like eta reduction
   
*)
let min_int = min_int

type t 
external say : int -> int = ""[@@bs.send.pipe:t]

let say = say

[@@@warning "-102"]
let v = Pervasives.compare
