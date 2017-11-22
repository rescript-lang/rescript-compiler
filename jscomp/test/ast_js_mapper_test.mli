

type 'a t = {
  xx : int ; 
  yy : string ; 
  zz : 'a * int 
} [@@bs.deriving {jsConverter }]

val searchForSureExists : (int * 'a) array -> int -> 'a


type a =   
  | A0 
  | A1
  | A2 
  | A3 
and b = 
  [ `b0 
  | `b1 
  | `b2
  | `b3 
  ]
[@@bs.deriving { jsConverter = newType }]  
