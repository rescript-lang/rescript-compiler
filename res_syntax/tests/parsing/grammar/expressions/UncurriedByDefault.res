let cApp = foo(3)
let uApp = foo(. 3)

let cFun = x => 3
let uFun = (.x) => 3
let mixFun = (a, .b, c) => (d, e, f) => (g, .h) => 4
let bracesFun = (. x) => y => x+y


@@uncurried

let cApp = foo(. 3)
let uApp = foo(3)

let cFun = (. x) => 3
let uFun = x => 3
let mixFun = (.a, b, .c) => (.d, .e, .f) => (.g, h) => 4
let bracesFun = x => (. y) => x+y
