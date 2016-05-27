

open Dom 
open Js 

let () = 
  match Null.to_opt @@ document##getElementById "x" with 
  | None -> log "hey"
  | Some v -> log "hi"
