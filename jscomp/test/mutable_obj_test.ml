



type u =
  < height : int [@bs.set] > Js.t




let f (x : u) =
  x##height#= 3  ;
  x##height * 2

let f ( x : < height : int [@bs.set{no_get}] > Js.t) =
  x##height#=3  



type v =
   < dec : int -> < x : int ; y : float > Js.t [@bs] [@bs.set]  >  Js.t

let f (x : v ) =
  x##dec#= (fun [@bs] x -> [%bs.obj {x ; y = float_of_int x }])   
