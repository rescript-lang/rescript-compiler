/*** Contains functions available in the global scope
    (`window` in a browser context) */

/** Identify an interval started by `Js.Global.setInterval`. */
type intervalId

/** Identify timeout started by `Js.Global.setTimeout`. */
type timeoutId

@val
/**
Clear an interval started by `Js.Global.setInterval`

```res example
/* API for a somewhat aggressive snoozing alarm clock */

let punchSleepyGuy = () => Js.log(\"Punch\")

let interval = ref(Js.Nullable.null)

let remind = () => {
  Js.log(\"Wake Up!\")
  punchSleepyGuy()
}

let snooze = mins =>
  interval := Js.Nullable.return(Js.Global.setInterval(remind, mins * 60 * 1000))

let cancel = () =>
  Js.Nullable.iter(interval.contents, (. intervalId) => Js.Global.clearInterval(intervalId))
```
*/
external clearInterval: intervalId => unit = "clearInterval"

@val
/**
Clear a timeout started by `Js.Global.setTimeout`.

```res example
/* A simple model of a code monkey's brain */

let closeHackerNewsTab = () => Js.log(\"close\")

let timer = ref(Js.Nullable.null)

let work = () => closeHackerNewsTab()

let procrastinate = mins => {
  Js.Nullable.iter(timer.contents, (. timer) => Js.Global.clearTimeout(timer))
  timer := Js.Nullable.return(Js.Global.setTimeout(work, mins * 60 * 1000))
}
```
*/
external clearTimeout: timeoutId => unit = "clearTimeout"

@val
/**
Repeatedly executes a callback with a specified interval (in milliseconds)
between calls. Returns a `Js.Global.intervalId` that can be passed to
`Js.Global.clearInterval` to cancel the timeout.

```res example
/* Will count up and print the count to the console every second */

let count = ref(0)

let tick = () => {
  count := count.contents + 1
  Js.log(Belt.Int.toString(count.contents))
}

Js.Global.setInterval(tick, 1000)
```
*/
external setInterval: (unit => unit, int) => intervalId = "setInterval"

@val
/**
Repeatedly executes a callback with a specified interval (in milliseconds)
between calls. Returns a `Js.Global.intervalId` that can be passed to
`Js.Global.clearInterval` to cancel the timeout.

```res example
/* Will count up and print the count to the console every second */

let count = ref(0)

let tick = () => {
  count := count.contents + 1
  Js.log(Belt.Int.toString(count.contents))
}

Js.Global.setIntervalFloat(tick, 1000.0)
```
*/
external setIntervalFloat: (unit => unit, float) => intervalId = "setInterval"

@val
/**
Execute a callback after a specified delay (in milliseconds). Returns a
`Js.Global.timeoutId` that can be passed to `Js.Global.clearTimeout` to cancel
the timeout.

```res example
/* Prints \"Timed out!\" in the console after one second */

let message = \"Timed out!\"

Js.Global.setTimeout(() => Js.log(message), 1000)
```
*/
external setTimeout: (unit => unit, int) => timeoutId = "setTimeout"

@val
/**
Execute a callback after a specified delay (in milliseconds). Returns a
`Js.Global.timeoutId` that can be passed to `Js.Global.clearTimeout` to cancel
the timeout.

```res example
/* Prints \"Timed out!\" in the console after one second */

let message = \"Timed out!\"

Js.Global.setTimeoutFloat(() => Js.log(message), 1000.0)
```
*/
external setTimeoutFloat: (unit => unit, float) => timeoutId = "setTimeout"

@val
/**
  URL-encodes a string.

  **see** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURI)
*/
external encodeURI: string => string = "encodeURI"

@val
/**
  Decodes a URL-enmcoded string produced by `encodeURI`

  **see** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/decodeURI)
*/
external decodeURI: string => string = "decodeURI"

@val
/**
  URL-encodes a string, including characters with special meaning in a URI.

  **see** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURIComponent)
*/
external encodeURIComponent: string => string = "encodeURIComponent"

@val
/**
  Decodes a URL-enmcoded string produced by `encodeURIComponent`

  **see** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/decodeURIComponent)
*/
external decodeURIComponent: string => string = "decodeURIComponent"
