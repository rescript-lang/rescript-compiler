type pair = int * int

type v = | A1 | A2 | B of int | C of int * int | D of pair

let a1 = A1
let a2 = A2

let b = B 34

let c = C (4,2)

let d = D (4,2)

let () = Js.log2 "a1" a1
let () = Js.log2 "a2" a2
let () = Js.log2 "b" b
let () = Js.log2 "c" c
let () = Js.log2 "d" d

let foo = function
| A1 -> 1
| A2 -> 2
| B n -> n
| C (n,m) -> n+m
| D (n,m) -> n+m

let fooA1 = function
| A1 -> 1
| _ -> 42

let fooC = function
| C (n,m) -> n+m
| _ -> 42

let switchNum = function
| 0 -> "0"
| 1 -> "1"
| 2 -> "2"
| _ -> "_"
