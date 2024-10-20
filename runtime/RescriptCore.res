include Global

module Array = Array
module Console = Console
module DataView = DataView
module Date = Date
module Dict = Dict
module Error = Error
module Float = Float
module Int = Int
module BigInt = BigInt
module Math = Math
module Null = Null
module Nullable = Nullable
module Object = Object
module Ordering = Ordering
module Promise = Promise
module RegExp = RegExp
module String = String
module Symbol = Symbol
module Type = Type
module JSON = JSON

module Iterator = Iterator
module AsyncIterator = AsyncIterator
module Map = Map
module WeakMap = WeakMap
module Set = Set
module WeakSet = WeakSet

module ArrayBuffer = ArrayBuffer
module TypedArray = TypedArray
module Float32Array = Float32Array
module Float64Array = Float64Array
module Int8Array = Int8Array
module Int16Array = Int16Array
module Int32Array = Int32Array
module Uint8Array = Uint8Array
module Uint16Array = Uint16Array
module Uint32Array = Uint32Array
module Uint8ClampedArray = Uint8ClampedArray
module BigInt64Array = BigInt64Array
module BigUint64Array = BigUint64Array

module Intl = Intl

@val external window: Dom.window = "window"
@val external document: Dom.document = "document"
@val external globalThis: {..} = "globalThis"

external null: Nullable.t<'a> = "#null"
external undefined: Nullable.t<'a> = "#undefined"
external typeof: 'a => Type.t = "#typeof"

/**
`import(value)` dynamically import a value or function from a ReScript
module. The import call will return a `promise`, resolving to the dynamically loaded
value.

## Examples

`Array.res` file:

```rescript
@send external indexOf: (array<'a>, 'a) => int = "indexOf"

let indexOfOpt = (arr, item) =>
  switch arr->indexOf(item) {
  | -1 => None
  | index => Some(index)
  }
```
In other file you can import the `indexOfOpt` value defined in `Array.res`

```rescript
let main = async () => {
  let indexOfOpt = await import(Array.indexOfOpt)
  let index = indexOfOpt([1, 2], 2)
  Console.log(index)
}
```

Compiles to:

```javascript
async function main() {
  var add = await import("./Array.mjs").then(function(m) {
    return m.indexOfOpt;
  });
  var index = indexOfOpt([1, 2], 2);
  console.log(index);
}
```
*/
external import: 'a => promise<'a> = "#import"

module Exn = Js.Exn
module Option = Option
module List = List
module Result = Result

type null<+'a> = Js.null<'a>

type undefined<+'a> = Js.undefined<'a>

type nullable<+'a> = Js.nullable<'a>

let panic = Error.panic
