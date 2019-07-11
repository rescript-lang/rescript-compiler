type resp

external set_okay : resp -> (_[@bs.as 200]) -> unit = "statusCode" [@@bs.set]
external set_hi : resp -> (_[@bs.as "hi"]) -> unit = "hi" [@@bs.set]

let f resp = set_okay resp ; set_hi resp
