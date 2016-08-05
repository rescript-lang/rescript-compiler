(* node.js readline class *)
type readline

(* bindings to event handler for 'close' and 'line' events *)
external on : readline -> 
    ([`close of unit -> unit 
    | `line of string -> unit] [@bs.string])
    -> unit = "" [@@bs.module "readline"]

(* register event handlers *)
let register rl =
  on rl (`close (fun event -> () ));
  on rl (`line (fun line -> print_endline line));