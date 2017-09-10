
type a =  
  { u_x : int}
[@@bs.deriving { accessors }]

type 'a b = {
  b_x  : int
}
and 'a c = {
  c_x : int 
}
[@@bs.deriving {accessors}]

type d = 
  | D_empty
  | D_int of int 
  | D_tuple of int * string
  | NewContent of string
  | D_tweak of (int * string)
  | Hei
and u = 
  | Hei
and h = {d : d ; h : h list; u_X : int}


and e = { d : d }
[@@bs.deriving {accessors}]


val v : d
val h : d list 

type hh = Xx of int 
[@@bs.deriving accessors]