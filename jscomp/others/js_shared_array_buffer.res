@@uncurried

/***
The underlying buffer that the typed arrays provide views of

**see** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/SharedArrayBuffer)
*/
type t = {
  byteLength: int,
  maxByteLength: int,
  growable: bool,
}

type makeOptions = {
  maxByteLength?: int
}

@new /** takes length. initializes elements to 0 */
external make: (int, makeOptions) => t = "SharedArrayBuffer"
let make = (length, ~maxByteLength=?) => make(length, { maxByteLength: ?maxByteLength })

/* ArrayBuffer.isView: seems pointless with a type system */

@get external byteLength: t => int = "byteLength"
@get external maxByteLength: t => int = "maxByteLength"
@get external growable: t => bool = "growable"

@send external slice: (t, ~start: int, ~end_: int) => t = "slice"
@send external sliceFrom: (t, int) => t = "slice"
