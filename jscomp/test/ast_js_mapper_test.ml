


type 'a t = {
  xx : int ; 
  yy : string ; 
  zz : 'a * int 
} 
[@@bs.deriving 

    jsConverter

]

let a = {xx = 3; yy = "x"; zz = 1,2}
let u = tToJs a
let v = tFromJs u

(** should also work*)
let vx = tFromJs [%obj{ xx = 3; yy = "2"; zz = 1,2; cc = 3}]



(* type u = 
  [ `D 
  | `C 
  | `f [@bs.as "x"]
  ]
  [@@bs.deriving jsConverter] *)

let rec searchAux i (xs : (int * _) array) (k : int) =  
  let (a,b) = Array.unsafe_get xs i in 
  if a = k then b 
  else searchAux (succ i) xs k 

let searchForSureExists xs k =   
  searchAux 0 xs k 


type a =   
  | A0 
  | A1 [@bs.as 3]
  | A2 
  | A3 
and b = 
  [ `b0 
  | `b1 
  | `b2
  | `b3 
  ]
[@@bs.deriving {jsConverter = newType}]  

let v = bToJs `b0

