
type u = 
  [ `D 
  | `C 
  | `f [@bs.as "x"]
  ]
  [@@bs.deriving jsMapper]



let (-~) f v =   
  match v with 
  | None -> "None"
  | Some x -> f x 
let v = 
  ( uToJs -~ uFromJs "x" ,
    uToJs -~ uFromJs "D",
    uToJs -~ uFromJs "C",
    uToJs -~ uFromJs "N")

;;Js.log (uToJs `f)

;; Js.log v 