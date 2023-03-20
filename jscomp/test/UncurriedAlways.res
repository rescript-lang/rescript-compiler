@@uncurried

let foo = (x, y) => x + y

let z = foo(. 3, 4)

let bar = (. x, y) => x + y

let b = bar(3, 4)

let w = 3->foo(4)

let a = 3->foo(. 4)

Js.log(a) // Test automatic uncurried application

let _ = Js.Array2.map([1], (. x) => x + 1)

let ptl = @res.partial foo(10) // force partial application

let foo2 = (x, y) => x + y
let bar2: _ => _ = foo2(_, 3)

let foo3 = (x, y, z) => x + y + z
let bar3: _ => _ = foo3(_, 3, 4)

type cmp = Jsx.component<int>
let q: cmp = _ => Jsx.null // Check that subtyping works past type definitions
