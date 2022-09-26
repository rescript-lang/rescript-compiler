let each = Js.import(Belt.List.forEach)

let eachInt = (list: list<int>, f: int => unit) =>
  Js.Promise.then_(each => list->each(f)->Js.Promise.resolve, each)

module type Belt = module type of Belt

let beltAsModule = Js.import(module(Belt: Belt))
