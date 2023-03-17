module Uncurried = {
  let raise = (. e) => raise(e)

  module List = {
    let map = (. l, f) => Belt.List.mapU(l, f)
  }
}

exception E

module StandardNotation = {
  open Uncurried

  let testRaise = () => raise(E)

  let l = List.map(.list{1, 2}, (. x) => x + 1)
  let partial = List.map(list{1, 2})
  let ll = partial((. x) => x + 1)

  let withOpts = (. ~x=3, y, ~z=4, w) => x + y + z + w
  type unc2 = (. ~z: int=?, int) => int
  let still2Args : unc2 = withOpts(4)
  let anInt = still2Args(~z=3)(. 5)
}

@@uncurried.swap

open Uncurried

let testRaise = () => raise(E)

let l = List.map(list{1, 2}, x => x + 1)
let partial = List.map(. list{1, 2})
let ll = partial(.x => x + 1)

let withOpts = (~x=3, y, ~z=4, w) => x + y + z + w
type unc2 = (~z: int=?, int) => int
let still2Args : unc2 = withOpts(. 4)
let anInt = still2Args(. ~z=3)(5)
