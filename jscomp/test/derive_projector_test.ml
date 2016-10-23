
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
  | Hei (* two hei derived, however, this hei is not accessible any more *)
and u = 
  | Hei
and h = {d : d ; h : h list}
and e = { d : d }

[@@bs.deriving {ffi}]



let v = d  @@ { d = d_int  3 }


let h = [
  d_empty ; 
  d_int 3 ; 
  d_tuple 3 "hgo";
  d_tweak (3,"hgo")
]

