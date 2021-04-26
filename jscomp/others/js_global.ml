(* Copyright (C) 2017 Hongbo Zhang, Authors of ReScript
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)



(** Contains functions available in the global scope
    (`window` in a browser context) *)


type intervalId
(** Identify an interval started by [` setInterval`]() *)

type timeoutId
(** Identify timeout started by [` setTimeout`]() *)


(**
  Clear an interval started by [` setInterval`]()

  ```
  (* API for a somewhat aggressive snoozing alarm clock *)

  let interval = ref Js.Nullable.null

  let remind () =
    Js.log "Wake Up!";
    IO.punchSleepyGuy ()

  let snooze mins =
    interval := Js.Nullable.return (Js.Global.setInterval remind (mins * 60 * 1000))

  let cancel () =
    Js.Nullable.iter !interval (fun[@bs] intervalId -> Js.Global.clearInterval intervalId)
  ```

  **see** [MDN](https://developer.mozilla.org/en-US/docs/Web/API/WindowOrWorkerGlobalScope/clearInterval)
*)
external clearInterval : intervalId -> unit = "clearInterval" [@@bs.val]


(**
  Clear a timeout started by [` setTimeout`]()

  ```
  (* A simple model of a code monkey's brain *)

  let timer = ref Js.Nullable.null

  let work () =
    IO.closeHackerNewsTab ()

  let procrastinate mins =
    Js.Nullable.iter !timer (fun[@bs] timer -> Js.Global.clearTimeout timer);
    timer := Js.Nullable.return (Js.Global.setTimeout work (mins * 60 * 1000))
  ```

  **see** [MDN](https://developer.mozilla.org/en-US/docs/Web/API/WindowOrWorkerGlobalScope/clearTimeout)
*)
external clearTimeout : timeoutId -> unit = "clearTimeout" [@@bs.val]


(**
  _Repeatedly_ executes a callback with a specified interval (in milliseconds) between calls

  **return** an [` intervalId`]() that can be passed to [` clearInterval`]() to cancel the timeout

  **see** [MDN](https://developer.mozilla.org/en-US/docs/Web/API/WindowOrWorkerGlobalScope/setInterval)

  ```
  (* Will count up and print the count to the console every second *)

  let count = ref 0

  let tick () =
    count := !count + 1; Js.log (string_of_int !count)

  let _ =
    Js.Global.setInterval tick 1000
  ```
*)
external setInterval : (unit -> unit) -> int -> intervalId = "setInterval" [@@bs.val]

(**
  _Repeatedly_ executes a callback with a specified interval (in milliseconds) between calls

  **return** an [` intervalId`]() that can be passed to [` clearInterval`]() to cancel the timeout

  **see** [MDN](https://developer.mozilla.org/en-US/docs/Web/API/WindowOrWorkerGlobalScope/setInterval)

  ```
  (* Will count up and print the count to the console every second *)

  let count = ref 0

  let tick () =
    count := !count + 1; Js.log (string_of_int !count)

  let _ =
    Js.Global.setIntervalFloat tick 1000.0
  ```
*)
external setIntervalFloat : (unit -> unit) -> float -> intervalId = "setInterval" [@@bs.val]


(**
  Execute a callback after a specified delay (in milliseconds)

  **return** a [` timeoutId`]() that can be passed to [` clearTimeout`]() to cancel the timeout

  **see** [MDN](https://developer.mozilla.org/en-US/docs/Web/API/WindowOrWorkerGlobalScope/setTimeout)

  ```
  (* Prints "Timed out!" in the console after one second *)

  let message = "Timed out!"

  let _ =
    Js.Global.setTimeout (fun () -> Js.log message) 1000
  ```
*)
external setTimeout : (unit -> unit) -> int -> timeoutId = "setTimeout" [@@bs.val]

(**
  Execute a callback after a specified delay (in milliseconds)

  **return** a [` timeoutId`]() that can be passed to [` clearTimeout`]() to cancel the timeout

  **see** [MDN](https://developer.mozilla.org/en-US/docs/Web/API/WindowOrWorkerGlobalScope/setTimeout)

  ```
  (* Prints "Timed out!" in the console after one second *)

  let message = "Timed out!"

  let _ =
    Js.Global.setTimeoutFloat (fun () -> Js.log message) 1000.0
  ```
*)
external setTimeoutFloat : (unit -> unit) -> float -> timeoutId = "setTimeout" [@@bs.val]

(**
  URL-encodes a string.

  **see** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURI)
*)
external encodeURI : string -> string = "encodeURI" [@@bs.val]


(**
  Decodes a URL-enmcoded string produced by `encodeURI`

  **see** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/decodeURI)
*)
external decodeURI : string -> string = "decodeURI" [@@bs.val]

(**
  URL-encodes a string, including characters with special meaning in a URI.

  **see** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURIComponent)
*)
external encodeURIComponent : string -> string = "encodeURIComponent" [@@bs.val]


(**
  Decodes a URL-enmcoded string produced by `encodeURIComponent`

  **see** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/decodeURIComponent)
*)
external decodeURIComponent : string -> string = "decodeURIComponent" [@@bs.val]
