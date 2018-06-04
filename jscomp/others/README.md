Belt is a data-structures and tools library optimized for the web.

```reason
let a = Belt.Array.make(10, 0);
let b = Belt.Array.map(a, (x) => x + 1);

module Comparator = Belt.Id.MakeComparable({
    type t = int;
    let cmp = Pervasives.compare
});

let c = Belt.Map.make(~id=(module Comparator));
let d = Belt.Map.set(c, 10, "Hello");
let e = Belt.Map.set(d, 11, "World!");

switch(Belt.Map.get(e, 11)) {
    | None => print_endline("Not possible, I'm pretty sure!")
    | Some(world) => print_endline("Hello " ++ world)
};

switch(Belt.Map.get(d, 11)) {
    | None => print_endline("Maps are immutable")
    | _ => print_endline("I will never print!")
}
```
