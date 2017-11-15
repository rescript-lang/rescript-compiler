


type 'a t = {
  xx : int ; 
  yy : string ; 
  zz : 'a * int 
} 
[@@bs.deriving 

    jsMapper

]

let a = {xx = 3; yy = "x"; zz = 1,2}
let u = tToJs a
let v = tFromJs u

(** should also work*)
let vx = tFromJs [%obj{ xx = 3; yy = "2"; zz = 1,2; cc = 3}]
;; Js.log 