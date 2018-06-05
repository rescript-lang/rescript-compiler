Belt is a data structures and utilities library that ships with BuckleScript and bsb-native, optimized for the web.

```ocaml
let a = Belt.Array.make 10 0
let b = Belt.Array.map a (fun x  -> x + 1)

module Comparator = Belt.Id.MakeComparable(struct 
  type t = int
  let cmp = Pervasives.compare 
end)

let c = Belt.Map.make ~id:(module Comparator)

let d = Belt.Map.set c 10 "Hello"
let e = Belt.Map.set d 11 "World!"

let _ =
  match Belt.Map.get e 11 with
  | None -> print_endline "Not possible, I'm pretty sure!"
  | Some world -> print_endline ("Hello " ^ world)

let _ =
  match Belt.Map.get d 11 with
  | None -> print_endline "Maps are immutable"
  | _ -> print_endline "I will never print!"
```

## Contributing

You'll need to build `bspp.exe` that is inside `../lib` by simply running `make -C ../lib bspp.exe`.

Then you can recompile all of belt to JS by running `make all`.
