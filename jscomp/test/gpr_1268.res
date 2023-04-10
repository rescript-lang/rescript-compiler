let f = (a, b, x, y) => a(x) |> \"+"(b(y))

@val external add: (int, int) => int = "add"

let f_add2 = (a, b, x, y) => a(x) |> add(b(y))

let f = (a, b, x, y) => a(x) + b(y)

let f1 = (a, b, x, y) => add(a(x), b(y))

let f2 = x => Js.log(x)

let f3 = x => x |> Js.log

let f4 = (x, y) => x |> add(y)
