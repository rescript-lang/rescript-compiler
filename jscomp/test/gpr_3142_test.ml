

type t = 
  [ `a [@bs.as "x"] 
  | `u [@bs.as "hi"]
  | `b (* [@bs.as {js|你|js} ] *)
  | `c (*[@bs.as {js|我|js}] *)
  ]
  [@@bs.deriving jsConverter]

let v,u = tToJs, tFromJs  