
type a =  
  { u_x : int}
[@@bs.deriving { projector }]

type 'a b = {
  b_x  : int
}
and 'a c = {
  c_x : int 
}
[@@bs.deriving {projector}]

type d = 
  | Int of int 

and e = { d : d }
[@@bs.deriving {projector}]
