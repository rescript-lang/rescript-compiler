
(*  should give a warning on unused attribute..   [@@bs.xx] *)


type readline
external on : 
  ([ `line of (string -> unit [@bs]) 
   | `close of (unit -> unit [@bs])] 
     [@bs.string]) ->
  'self = 
  "on" [@@bs.send.pipe:readline as 'self]
let u rl =
  rl
  |> on (`line (fun [@bs] x -> Js.log x ))
  |> on (`close (fun [@bs] () -> Js.log "finished"))    




external send : string -> 'self   = "send" [@@bs.send.pipe: [%bs.obj: < hi : int > ] as 'self]


let xx h : int  =
  h
  |> send   "x"
  |> (fun x -> x ## hi)    

let yy h =
  h
  |> send "x"    
