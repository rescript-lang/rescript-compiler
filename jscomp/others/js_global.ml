(** Contains functions available in the global scope ([window] in a browser context) *)

type intervalId
(** identifies an interval started by {! setInterval} *)

type timeoutId
(** identifies a timeout started by {! setTimeout} *)

external clearInterval : intervalId -> unit = "" [@@bs.val]
(** clears an interval started by {! setInterval}

@see <https://developer.mozilla.org/en-US/docs/Web/API/WindowOrWorkerGlobalScope/clearInterval> MDN

@example {[
(* API for a somewhat aggressive snoozing alarm clock *)

let interval = Js.Undefined.empty

let remind () =
  Js.log "Wake Up!";
  IO.punchSleepyGuy ()

let snooze mins =
  interval := Js.Undefined.return \@\@ Js.Global.setInterval remind (mins * 60 * 1000)

let cancel () =
  Js.Undefined.iter Js.Global.clearInterval !interval
]}
*)


external clearTimeout : timeoutId -> unit = "" [@@bs.val]
(** clears a timeout started by {! setTimeout}

@see <https://developer.mozilla.org/en-US/docs/Web/API/WindowOrWorkerGlobalScope/clearTimeout> MDN

@example {[
(* A simple model of a code monkey's brain *)

let timer = Js.Undefined.empty

let work () =
  IO.closeHackerNewsTab ()

let procrastinate mins =
  Js.Undefined.iter fun Js.Global.clearTimeout !timer
  Js.Global.setTimeout work (mins * 60 * 1000)
]}
*)


external setInterval : ((unit -> unit) [@bs.uncurry]) -> int -> intervalId = "" [@@bs.val]
(** {i repeatedly} executes a callback with a specified interval (in milliseconds) between calls

{b returns} an {! intervalId} that can be passed to {! clearInterval} to cancel the timeout

@see <https://developer.mozilla.org/en-US/docs/Web/API/WindowOrWorkerGlobalScope/setInterval> MDN

@example {[
(* Will count up and print the count to the console every second *)

let count = ref 0

let tick () =
  count := !count + 1; Js.log (string_of_int !count)

let _ =
  Js.Global.setInterval tick 1000
]}
*)


external setTimeout : ((unit -> unit) [@bs.uncurry]) -> int -> timeoutId = "" [@@bs.val]
(** executes a callback after a specified delay (in milliseconds)

{b returns} a {! timeoutId} that can be passed to {! clearTimeout} to cancel the timeout

@see <https://developer.mozilla.org/en-US/docs/Web/API/WindowOrWorkerGlobalScope/setTimeout> MDN

@example {[
(* Prints "Timed out!" in the console after one second *)

let message = "Timed out!"

let _ =
  Js.Global.setTimeout (fun () -> Js.log message) 1000
]}
*)


external encodeURI : string -> string = "" [@@bs.val]
(** URL-encodes a string.

@see <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURI> MDN
*)

external decodeURI : string -> string = "" [@@bs.val]
(** Decodes a URL-enmcoded string produced by [encodeURI]

@see <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/decodeURI> MDN
*)

external encodeURIComponent : string -> string = "" [@@bs.val]
(** URL-encodes a string, including characters with special meaning in a URI.

@see <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURIComponent> MDN
*)

external decodeURIComponent : string -> string = "" [@@bs.val]
(** Decodes a URL-enmcoded string produced by [encodeURIComponent]

@see <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/decodeURIComponent> MDN
*)