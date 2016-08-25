(* node.js readline class *)
type readline

(* bindings to event handler for 'close' and 'line' events *)
external on : 
    ([`close of unit -> unit 
    | `line of string -> unit] [@bs.string])
    -> 'this = "" [@@bs.send.pipe: readline as 'this] 

(* register event handlers *)
let register rl =
  rl 
  |> on (`close (fun event -> () ))
  |> on (`line (fun line -> print_endline line))