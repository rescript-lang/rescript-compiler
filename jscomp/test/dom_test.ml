

open Dom 
open Js 

let () = 
  match Null.to_opt @@ document##getElementById "x" with 
  | None -> log "hey"
  | Some v -> 
    Js.log (v##nodeName, v##parentNode(* e, v##xx *))

