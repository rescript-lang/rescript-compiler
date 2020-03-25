
[@@@bs.config{flags = 
[|
  "-bs-diagnose"
|]}]


external
 hi: (unit -> unit [@bs.uncurry 0]) -> int = "hi" [@@bs.val]

let f_01 () = hi (fun (() as x) -> if x = () then Js.log "x" ) (* FIXME: not inlined *)



let u x = 
  match () with 
  | () when x > 3 -> 1 
  | () when x < 2 -> 2
  | () when x > 4 -> 0
  | () -> 3 