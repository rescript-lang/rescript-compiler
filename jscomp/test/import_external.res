@module("a")
external makeA: string = "default"

let f8 = Js.import(makeA)
