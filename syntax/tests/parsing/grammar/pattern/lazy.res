let lazy x = ()
let lazy x as l = ()
let lazy (x as l) = ()
let (lazy x) = ()
let (lazy x) as l = ()
let (lazy x as l)  = ()
let (lazy (x as l))  = ()
let (lazy (x: int)) = ()
let lazy (x: int) = ()
let (lazy x: Lazy.t<int>) = ()
let (lazy x: Lazy.t<int>) as l = ()
let (lazy (x as l): Lazy.t<int>) = ()

let lazy exception x = ()
let lazy (exception x) = ()

switch x {
| lazy foo => ()
| lazy foo as l => ()
| lazy (foo as l) => ()
| (lazy x: Lazy.t<int>) => ()
}

let f = (lazy x) => ()
let f = (lazy x as l) => ()
let f = (lazy (x as l)) => ()
let f = ((lazy x)) => ()
let f = ((lazy x) as l) => ()
let f = ((lazy (x as l))) => ()
let f = (lazy x: Lazy.t<int>) => ()
let f = ((lazy x: Lazy.t<int>) as x) => ()
let f = ((lazy (x: Lazy.t<int>) as x) ) => ()
let f = ((lazy ((x: Lazy.t<int>) as l) ) ) => ()
let f = ((lazy x: Lazy.t<int>)) => ()

for lazy x in z to g { () }
for lazy x as l in z to g { () }
for lazy (x as l) in z to g { () }
for (lazy x in z to g) { () }
for (lazy x as l in z to g) { () }
for (lazy (x as l) in z to g) { () }
for ((lazy x) in z to g) { () }
for ((lazy x) as l in z to g) { () }
for ((lazy x as l)  in z to g) { () }
for ((lazy (x as l))  in z to g) { () }
for ((lazy x: Lazy.t<int>) in z to g) { () }
for ((lazy x: Lazy.t<int>) as l in z to g) { () }
for ((lazy (x: Lazy.t<int>) as l) in z to g) { () }
for ((lazy ((x: Lazy.t<int>) as l)) in z to g) { () }
