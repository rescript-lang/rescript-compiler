

let x = true [@@bs.inline]

let f = "hello" [@@bs.inline]

let f1 = {js|a|js} [@@bs.inline]

let f2  = {js|中文|js} [@@bs.inline]
(* Do we need fix 
  let f2 : string = blabla
*)

module N : sig 
  val f3 : string [@@bs.inline {js|中文|js} ]
end = struct 
  let f3 = {js|中文|js} [@@bs.inline]
end 

module N1 = functor () -> struct 
  let f4 = {js|中文|js} [@@bs.inline]
  let xx = 3e-6 [@@bs.inline]
  let xx0 = 3e-6
end 
let h = f 

let hh = f ^ f 

open N

module H = N1 ()
open H
let a,b,c,d,e = 
  f,f1,f2,f3,f4

let f5 = true [@@bs.inline]  

let f6 = 1 [@@bs.inline]

let f7 = 1L [@@bs.inline]

let f9 = 100L [@@bs.inline] 

let v = 100L [@@bs.inline]
let u = 1L [@@bs.inline] 

let () = Js.log (xx,xx0)