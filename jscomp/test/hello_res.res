@@config({
  flags: ["-dparsetree"],
})

let b = List.length(list{1, 2, 3})
let a = b - 1
Js.log("hello, res")

type t = {"x": int}

type h = {...t, "y": string}

let u01: h = {"x": 3, "y": "x"}
let u: t = {"x": 3}

let h = u["x"]

%%private(let {length, cons} = module(List))

%%private(let {length, cons} = module(List))

%%private(let (a, b) = (1, 2))

let {length: len, cons: c} = module(List)

module H = {
  module H1 = {
    let v = 3
  }
}
let u = {
  let {length: l, cons} = module(List)
  cons(l(list{1, 2, 3}), list{})
}

let h = {
  let {v} = module(H.H1)
  Js.log(v)
}

let to = 3
let downto = 1

Js.log([to, downto])
