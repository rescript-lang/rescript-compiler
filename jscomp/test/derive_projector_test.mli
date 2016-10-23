
type a =  
  { u_x : int}
[@@bs.deriving { ffi }]

type 'a b = {
  b_x  : int
}
and 'a c = {
  c_x : int 
}
[@@bs.deriving {ffi}]

type d = 
  | D_empty
  | D_int of int 
  | D_tuple of int * string 
  | D_tweak of (int * string)
  | Hei
and u = 
  | Hei
and h = {d : d ; h : h list}

and e = { d : d }
[@@bs.deriving {ffi}]


val v : d
val h : d list 
