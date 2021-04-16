
(*  should give a warning on unused attribute..   [@@bs.xx] *)


type readline
external on : (readline as 'self) ->
  ([ `line of (string -> unit [@bs]) 
   | `close of (unit -> unit [@bs])] 
     [@bs.string]) ->
  'self = 
  "on" [@@send]
let u rl =
  rl
  |.on (`line (fun [@bs] x -> Js.log x ))
  |. on (`close (fun [@bs] () -> Js.log "finished"))    




external send : (< hi : int >  as 'self) ->  string -> 'self   = "send" [@@send ]


let xx h : int  =
  h
  |. send   "x"
  |. (fun x -> x # hi)    

let yy h =
  h
  |. send "x"    
