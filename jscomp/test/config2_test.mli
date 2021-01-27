

class type v = object
  method hey : int -> int -> int 
end [@bs]

class type v2 = object
  method hey : int -> int -> int 
end [@bs]

type vv = 
   < 
    hey : int -> int -> int [@bs]
  >  Js.t

type vv2 = 
   < 
    hey : int -> int -> int [@bs]
  > Js.t


val test_v : v Js.t -> int
val test_vv : vv -> int 
