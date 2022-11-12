let cApp = foo(3)
let uApp = foo(. 3)

let cFun = x => 3
let uFun = (.x) => 3
let mixFun = (a, .b, c) => (d, e, f) => (g, .h) => 4
let bracesFun = (. x) => y => x+y
let cFun2 = (x, y) => 3
let uFun2 = (. x, y) => 3

type cTyp = string => int
type uTyp = (. string) => int
type mixTyp = (string, .string, string) => (string, string, string) => (string, .string) => int
type bTyp = (. string) => string => int
// type cTyp2 = (string, string) => int
// type uTyp2 = (.string, string) => int

@@uncurried

let cApp = foo(. 3)
let uApp = foo(3)

let cFun = (. x) => 3
let uFun = x => 3
let mixFun = (.a) => (b, c) => (.d, e, f) => (.g) => h => 4
let bracesFun = x => (. y) => x+y
let cFun2 = (. x, y) => 3
let uFun2 = (x, y) => 3
let cFun2Dots = (.x, .y) => 3 // redundant dot on y

type cTyp = (. string) => int
type uTyp = string => int
type mixTyp = (.string, string, .string) => (.string, .string, .string) => (.string, string) => int
type bTyp = string => (. string) => int
// type cTyp2 = (.string, string) => int
// type uTyp2 = (string, string) => int
