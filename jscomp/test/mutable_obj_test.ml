



type u =
  < height : int [@bs.set] > Js.t




let f (x : u) =
  x##height#= 3  ;
  x##height * 2

let f ( x : < height : int [@bs.set{no_get}] > Js.t) =
  x##height#=3  

