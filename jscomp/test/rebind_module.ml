

type a = ..
type b = ..

type a += A 
(* A is shadowed so it will available any more,
   it seems record disamibigution does not 
   work with extensible type
*)

type b += A 
