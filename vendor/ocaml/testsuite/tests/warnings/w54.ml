
let f = (fun x -> x) [@inline] [@inline never]
let g = (fun x -> x) [@inline] [@something_else] [@ocaml.inline]

let h x = (g [@inlined] [@ocaml.inlined never]) x

let v = ((fun x -> x) [@inline] [@inlined]) 1 (* accepted *)

let i = ((fun x -> x) [@inline]) [@@inline]
