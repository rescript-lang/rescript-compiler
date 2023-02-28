@genType
let null0 = (x: Js.null<int>) => x

@genType
let null1 = (x: Null.t<int>) => x

@genType
let nullable0 = (x: Js.nullable<int>) => x

@genType
let nullable1 = (x: Nullable.t<int>) => x

@genType
let undefined0 = (x: Js.undefined<int>) => x

@genType
let undefined1 = (x: Undefined.t<int>) => x

@genType
let dict0 = (x: Js.Dict.t<string>) => x

@genType
let dict1 = (x: Dict.t<string>) => x

@genType
let promise0 = (x: promise<string>) => x

@genType
let promise1 = (x: Promise.t<string>) => x

@genType
let date0 = (x: Js.Date.t) => x

@genType
let date1 = (x: Date.t) => x

@genType
let bigint0 = (x: Js.Types.bigint_val) => x

@genType
let bigint1 = (x: BigInt.t) => x

@genType
let regexp0 = (x: Js.Re.t) => x

@genType
let regexp1 = (x: RegExp.t) => x

module Map = Map_

@genType
let map1 = (x: Map.t<string, int>) => x

@genType
let weakmap1 = (x: WeakMap.t<array<int>, int>) => x
