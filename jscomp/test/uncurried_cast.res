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
}

@@uncurried

open Uncurried

let testRaise = () => raise(E)

let l = List.map(list{1, 2}, x => x + 1)

let partial = List.map(. list{1, 2})

let ll = partial(.x => x + 1)
