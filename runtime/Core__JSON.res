@unboxed
type rec t = Js.Json.t =
  | Boolean(bool)
  | @as(null) Null
  | String(string)
  | Number(float)
  | Object(Core__Dict.t<t>)
  | Array(array<t>)

@unboxed
type replacer = Keys(array<string>) | Replacer((string, t) => t)

@raises @val external parseExn: (string, ~reviver: (string, t) => t=?) => t = "JSON.parse"
@deprecated("Use `parseExn` with optional parameter instead") @raises @val
external parseExnWithReviver: (string, (string, t) => t) => t = "JSON.parse"

@val external stringify: (t, ~replacer: replacer=?, ~space: int=?) => string = "JSON.stringify"
@deprecated("Use `stringify` with optional parameter instead") @val
external stringifyWithIndent: (t, @as(json`null`) _, int) => string = "JSON.stringify"
@deprecated("Use `stringify` with optional parameter instead") @val
external stringifyWithReplacer: (t, (string, t) => t) => string = "JSON.stringify"
@deprecated("Use `stringify` with optional parameters instead") @val
external stringifyWithReplacerAndIndent: (t, (string, t) => t, int) => string = "JSON.stringify"
@deprecated("Use `stringify` with optional parameter instead") @val
external stringifyWithFilter: (t, array<string>) => string = "JSON.stringify"
@deprecated("Use `stringify` with optional parameters instead") @val
external stringifyWithFilterAndIndent: (t, array<string>, int) => string = "JSON.stringify"

@raises @val
external stringifyAny: ('a, ~replacer: replacer=?, ~space: int=?) => option<string> =
  "JSON.stringify"
@deprecated("Use `stringifyAny` with optional parameter instead") @raises @val
external stringifyAnyWithIndent: ('a, @as(json`null`) _, int) => option<string> = "JSON.stringify"
@deprecated("Use `stringifyAny` with optional parameter instead") @raises @val
external stringifyAnyWithReplacer: ('a, (string, t) => t) => option<string> = "JSON.stringify"
@deprecated("Use `stringifyAny` with optional parameters instead") @raises @val
external stringifyAnyWithReplacerAndIndent: ('a, (string, t) => t, int) => option<string> =
  "JSON.stringify"
@deprecated("Use `stringifyAny` with optional parameter instead") @raises @val
external stringifyAnyWithFilter: ('a, array<string>) => string = "JSON.stringify"
@deprecated("Use `stringifyAny` with optional parameters instead") @raises @val
external stringifyAnyWithFilterAndIndent: ('a, array<string>, int) => string = "JSON.stringify"

module Classify = {
  type t =
    | Bool(bool)
    | Null
    | String(string)
    | Number(float)
    | Object(Core__Dict.t<t>)
    | Array(array<t>)

  @val external _internalClass: 'a => string = "Object.prototype.toString.call"
  external _asBool: 'a => bool = "%identity"
  external _asString: 'a => string = "%identity"
  external _asFloat: 'a => float = "%identity"
  external _asArray: 'a => array<Js.Json.t> = "%identity"
  external _asDict: 'a => Core__Dict.t<Js.Json.t> = "%identity"

  let classify = value => {
    switch _internalClass(value) {
    | "[object Boolean]" => Bool(_asBool(value))
    | "[object Null]" => Null
    | "[object String]" => String(_asString(value))
    | "[object Number]" => Number(_asFloat(value))
    | "[object Array]" => Array(_asArray(value))
    | _ => Object(_asDict(value))
    }
  }
}

module Encode = {
  external bool: bool => t = "%identity"
  external null: t = "#null"
  external string: string => t = "%identity"
  external int: int => t = "%identity"
  external float: float => t = "%identity"
  external object: Core__Dict.t<t> => t = "%identity"
  external array: array<t> => t = "%identity"
}

module Decode = {
  let bool = (json: t) =>
    Core__Type.typeof(json) === #boolean ? Some((Obj.magic(json): bool)) : None
  let null = (json: t) => Obj.magic(json) === Core__Null.null ? Some(Core__Null.null) : None
  let string = (json: t) =>
    Core__Type.typeof(json) === #string ? Some((Obj.magic(json): string)) : None
  let float = (json: t) =>
    Core__Type.typeof(json) === #number ? Some((Obj.magic(json): float)) : None
  let object = (json: t) =>
    if (
      Core__Type.typeof(json) === #object &&
      !Core__Array.isArray(json) &&
      !(Obj.magic(json) === Core__Null.null)
    ) {
      Some((Obj.magic(json): Core__Dict.t<t>))
    } else {
      None
    }
  let array = (json: t) => Core__Array.isArray(json) ? Some((Obj.magic(json): array<t>)) : None
}
