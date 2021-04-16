type readline
external on : readline ->
  ([ `line of string -> unit 
   | `close of unit -> unit] 
    [@string] ) ->
  readline = 
  "on" [@@send]
let register rl =
  rl
  |. on (`line (fun  x -> Js.log x ))
  |. on (`close (fun () -> Js.log "finished"))    

