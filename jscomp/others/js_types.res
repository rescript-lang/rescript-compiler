/*** */

/** Js symbol type only available in ES6 */
type symbol

/** Js bigint type only available in ES2020 */
type bigint_val = Js_bigint.t

type obj_val
/** This type has only one value `undefined` */
type undefined_val

/** This type has only one value `null` */
type null_val

type function_val

type rec t<_> =
  | Undefined: t<undefined_val>
  | Null: t<null_val>
  | Boolean: t<bool>
  | Number: t<float>
  | String: t<string>
  | Function: t<function_val>
  | Object: t<obj_val>
  | Symbol: t<symbol>
  | BigInt: t<bigint_val>

type tagged_t =
  | JSFalse
  | JSTrue
  | JSNull
  | JSUndefined
  | JSNumber(float)
  | JSString(string)
  | JSFunction(function_val)
  | JSObject(obj_val)
  | JSSymbol(symbol)
  | JSBigInt(bigint_val)

let classify = (x: 'a): tagged_t => {
  let ty = Js.typeof(x)
  if ty == "undefined" {
    JSUndefined
  } else if x === Obj.magic(Js_null.empty) {
    JSNull
  } else if ty == "number" {
    JSNumber(Obj.magic(x))
  } else if ty == "bigint" {
    JSBigInt(Obj.magic(x))
  } else if ty == "string" {
    JSString(Obj.magic(x))
  } else if ty == "boolean" {
    if Obj.magic(x) == true {
      JSTrue
    } else {
      JSFalse
    }
  } else if ty == "symbol" {
    JSSymbol(Obj.magic(x))
  } else if ty == "function" {
    JSFunction(Obj.magic(x))
  } else {
    JSObject(Obj.magic(x))
  }
}

let test = (type a, x: 'a, v: t<a>): bool =>
  switch v {
  | Number => Js.typeof(x) == "number"
  | Boolean => Js.typeof(x) == "boolean"
  | Undefined => Js.typeof(x) == "undefined"
  | Null => x === Obj.magic(Js_null.empty)
  | String => Js.typeof(x) == "string"
  | Function => Js.typeof(x) == "function"
  | Object => Js.typeof(x) == "object"
  | Symbol => Js.typeof(x) == "symbol"
  | BigInt => Js.typeof(x) == "bigint"
  }
