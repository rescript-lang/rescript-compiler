

external f : int -> int array -> int = "Math.max" 
  [@@bs.splice] [@@bs.val]

;; f 1 [||]

external send : int -> int array -> int = "" 
  [@@bs.splice] [@@bs.send]

let f a b =   
  a |. send [|b|]


#if 0 then
let f1 c =  f 1 c 
;; f1  [|2;3|] 
#end
