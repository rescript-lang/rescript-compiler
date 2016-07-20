

external f : ([`on_closed | `on_open | `in_ [@bs.name "in"]]
                [@bs.stringify]) -> unit -> int = 
  "hey" [@@bs.call]

let u = f `on_open

let option = `on_closed 

let v = f option 

let ff h  = f h ()

let xx = f `in_


