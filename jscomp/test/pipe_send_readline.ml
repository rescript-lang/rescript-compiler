
(*  should give a warning on unused attribute..   [@@bs.xx] *)


type readline
external on : 
  ([ `line of (string -> unit [@bs]) 
   | `close of (unit -> unit [@bs])] 
     [@bs.string]) ->
  readline = 
  "" [@@bs.send.pipe:readline]
let u rl =
  rl
  |> on (`line (fun [@bs] x -> Js.log x ))
  |> on (`close (fun [@bs] () -> Js.log "finished"))    
  
