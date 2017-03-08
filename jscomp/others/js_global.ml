type intervalId
type timeoutId

external clearInterval : intervalId -> unit = "" [@@bs.val]
external clearTimeout : timeoutId -> unit = "" [@@bs.val]

external setInterval : (unit -> unit) -> int -> intervalId = "" [@@bs.val]
external setTimeout : (unit -> unit) -> int -> timeoutId = "" [@@bs.val]