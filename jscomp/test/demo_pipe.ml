type readline

external on :
  ([`line of string -> unit | `close of unit -> unit][@bs.string]) -> readline
  = "on"
  [@@bs.send.pipe: readline]

let register rl =
  rl
  |> on (`line (fun x -> Js.log x))
  |> on (`close (fun () -> Js.log "finished"))
