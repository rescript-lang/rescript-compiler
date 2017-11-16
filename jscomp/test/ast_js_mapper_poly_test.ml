
type u = 
  [ `D 
  | `C 
  | `f [@bs.as "x"]
  ]
  [@@bs.deriving jsMapper]


;;Js.log (uToJs `f)