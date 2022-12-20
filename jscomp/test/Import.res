let eachIntAsync = async (list: list<int>, f: int => unit) => {
  list->(await Js.import(Belt.List.forEach))(f)
}

let eachIntLazy = (list: list<int>, f: int => unit) =>
  Js.import(Belt.List.forEach) |> Js.Promise.then_(each => list->each(f)->Js.Promise.resolve)

let _ = list{1, 2, 3}->eachIntLazy(n => Js.log2("lazy", n))
let _ = list{1, 2, 3}->eachIntAsync(n => Js.log2("async", n))

module type BeltList = module type of Belt.List
let beltAsModule = await Js.import(module(Belt.List: BeltList))

// module type BeltList0 = module type of Belt.List
// module M = unpack(@res.await Js.import(module(Belt.List: BeltList0)))
module M = @res.await Belt.List
let each = M.forEach