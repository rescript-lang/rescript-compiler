@@uncurried

@scope("Atomics") external isLockFree: int => bool = "isLockFree"

module MakeOps = (TA: Js_typed_array2.S) => {
  @scope("Atomics") external add: (TA.t, int, int) => TA.elt = "add"
  @scope("Atomics") external sub: (TA.t, int, int) => TA.elt = "sub"
  @scope("Atomics") external and_: (TA.t, int, int) => TA.elt = "and"
  @scope("Atomics") external or_: (TA.t, int, int) => TA.elt = "or"
  @scope("Atomics") external xor: (TA.t, int, int) => TA.elt = "xor"
  @scope("Atomics") external load: (TA.t, ~index: int) => TA.elt = "load"
  @scope("Atomics") external store: (TA.t, ~index: int, ~value: TA.elt) => TA.elt = "store"
  @scope("Atomics") external exchange: (TA.t, ~index: int, ~value: TA.elt) => TA.elt = "exchange"
  @scope("Atomics") external compareExchange: (TA.t, ~index: int, ~expectedValue: TA.elt, ~replacementValue: TA.elt) => TA.elt = "compareExchange"
}

module MakeFutex = (TA: Js_typed_array2.S) => {
  // These operations are supported only when TA is Int32Array or BigInt64Array, and also underlying buffer is SharedArrayBuffer

  @scope("Atomics") external wait: (TA.t, ~index: int, ~value: TA.elt) => [#ok | #"not-equal"] = "wait"
  @scope("Atomics") external waitWithTimeout: (TA.t, ~index: int, ~value: TA.elt, ~timeout: int) => [#ok | #"not-equal" | #"timed-out"] = "wait"

  @scope("Atomics") external waitAsync: (TA.t, ~index: int, ~value: TA.elt) => promise<[#ok | #"not-equal"]> = "waitAsync"
  @scope("Atomics") external waitAsyncWithTimeout: (TA.t, ~index: int, ~value: TA.elt, ~timeout: int) => promise<[#ok | #"not-equal" | #"timed-out"]> = "waitAsync"

  @scope("Atomics") external notify: (TA.t, ~index: int) => int = "notify"
  @scope("Atomics") external notifyForCount: (TA.t, ~index: int, ~count: int) => int = "notify"
}

module Int8Array = MakeOps(Js_typed_array2.Int8Array)

module Uint8Array = MakeOps(Js_typed_array2.Uint8Array)

module Int16Array = MakeOps(Js_typed_array2.Int16Array)

module Uint16Array = MakeOps(Js_typed_array2.Uint16Array)

module Int32Array = {
  include MakeOps(Js_typed_array2.Int32Array)
  include MakeFutex(Js_typed_array2.Int32Array)
}

module Uint32Array = MakeOps(Js_typed_array2.Uint32Array)

/** TODO: uncomment this when ready
module BigInt64Array = {
  include MakeOps(Js_typed_array2.BigInt64Array)
  include MakeFutex(Js_typed_array2.BigInt64Array)
}

module BigUint64Array = MakeOps(Js_typed_array2.BigUint64Array)
*/

module Float32Array = MakeOps(Js_typed_array2.Float32Array)

module Float64Array = MakeOps(Js_typed_array2.Float64Array)
