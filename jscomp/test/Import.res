let eachIntAsync = async (list: list<int>, f: int => unit) => {
  list->(await Js.import(Belt.List.forEach))(f)
}

let eachIntLazy = (list: list<int>, f: int => unit) =>
  Js.import(Belt.List.forEach) |> Js.Promise.then_(each => list->each(f)->Js.Promise.resolve)

let _ = list{1, 2, 3}->eachIntLazy(n => Js.log2("lazy", n))
let _ = list{1, 2, 3}->eachIntAsync(n => Js.log2("async", n))

module type BeltList = module type of Belt.List
let beltAsModule = await Js.import(module(Belt.List: BeltList))

module List2 = @res.await Belt.List
let v = List2.v
let w: Belt.List.variant = v

module Local = {
  type variant = V
  let v = V
}

module Local2 = @res.await Local
let v = Local2.v
let w: Local.variant = v
