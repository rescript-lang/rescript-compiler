

type 'a t = {
  xx : int ; 
  yy : string ; 
  zz : 'a * int 
} [@@bs.deriving {jsMapper }]

val searchForSureExists : (int * 'a) array -> int -> 'a