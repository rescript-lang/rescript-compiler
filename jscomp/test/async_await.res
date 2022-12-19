let withAnnotations1 = async () : promise<int> => 3
let withAnnotations2 = async () : promise<int> => 3
let withAnnotations3 = async () : promise<int> => (3:int)

@@uncurried

let next = n => n + 1
let useNext = async () => next(3)

module type Impl = {
  let get: string => Js.Promise.t<string>
}

module Make = (I: Impl) => {
  let get = async key => await I.get(key)
}
