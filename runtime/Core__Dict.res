type t<'a> = Js.Dict.t<'a>

@get_index external getUnsafe: (t<'a>, string) => 'a = ""
@get_index external get: (t<'a>, string) => option<'a> = ""
@set_index external set: (t<'a>, string, 'a) => unit = ""
@val external delete: 'a => unit = "delete"

let delete = (dict, string) => {
  delete(get(dict, string))
}

@obj external make: unit => t<'a> = ""

@val external fromArray: array<(string, 'a)> => t<'a> = "Object.fromEntries"
@val external fromIterator: Core__Iterator.t<(string, 'a)> => t<'a> = "Object.fromEntries"

@val external toArray: t<'a> => array<(string, 'a)> = "Object.entries"

@val external keysToArray: t<'a> => array<string> = "Object.keys"

@val external valuesToArray: t<'a> => array<'a> = "Object.values"

@val external assign: (t<'a>, t<'a>) => t<'a> = "Object.assign"

@val external copy: (@as(json`{}`) _, t<'a>) => t<'a> = "Object.assign"

let forEach = (dict, f) => {
  dict->valuesToArray->Core__Array.forEach(value => f(value))
}

let forEachWithKey = (dict, f) => {
  dict->toArray->Core__Array.forEach(((key, value)) => f(value, key))
}

let mapValues = (dict, f) => {
  let target = make()
  dict->forEachWithKey((value, key) => {
    target->set(key, f(value))
  })
  target
}
