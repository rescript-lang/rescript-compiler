
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

type v = 
  | A0 
  | A1 [@bs.as 3]
  | A2
  | A3 
[@@bs.deriving jsMapper]

let s = function 
  | A0 -> "A0"
  | A1 -> "A1"
  | A2 -> "A2"
  | A3 -> "A3"

;; Js.log (vToJs A3)

;; Js.log (Array.map ( fun x -> match vFromJs x with None -> "None" | Some x -> s x )
    [|0;1;2;3;4;5|])

type v1 =     
  | B0 
  | B1 
  | B2 
  | B3 
  | B4 
  | B5 
  [@@bs.deriving jsMapper]

(** TODO: add jsType support *)  
type v2 =  
  | C0  [@bs.as 2 ]
  | C1
  | C2 
  | C3 
  | C4
  | C5 
  [@@bs.deriving jsMapper ]


;;  
Js.log (Array.map v2ToJs [|C0; C1; C2 ; C3 ; C4; C5 |])

;;
let xs  = (Array.map v2ToJs [|C0; C1; C2 ; C3 ; C4; C5 |])
;;
Js.log (Array.map v2FromJs (Array.map succ xs))