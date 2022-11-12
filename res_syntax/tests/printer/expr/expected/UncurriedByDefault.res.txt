let cApp = foo(3)
let uApp = foo(. 3)

// let cFun = x => 3
// let uFun = (.x) => 3
// let mixFun = (a, .b, c) => (d, e, f) => (g, .h) => 4
// let bracesFun = (. x) => y => x+y

// type cTyp = string => int
// type uTyp = (. string) => int
// type mixTyp = (string, .string, string) => (string, string, string) => (string, .string) => int
// type bTyp = (. string) => string => int

@@uncurried

let cApp = foo(. 3)
let uApp = foo(3)

// let cFun = (. x) => 3
// let uFun = x => 3
// let mixFun = (.a, b, .c) => (.d, .e, .f) => (.g, h) => 4
// let bracesFun = x => (. y) => x+y

// type cTyp = (. string) => int
// type uTyp = string => int
// type mixTyp = (.string, string, .string) => (.string, .string, .string) => (.string, string) => int
// type bTyp = string => (. string) => int
