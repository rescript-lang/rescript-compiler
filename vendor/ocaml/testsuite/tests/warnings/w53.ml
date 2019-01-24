
let h x = x [@inline] (* rejected *)
let h x = x [@ocaml.inline] (* rejected *)

let i x = x [@inlined] (* rejected *)
let j x = x [@ocaml.inlined] (* rejected *)
let k x = (h [@inlined]) x (* accepted *)
let k' x = (h [@ocaml.inlined]) x (* accepted *)
let l x = h x [@inlined] (* rejected *)

let m x = x [@tailcall] (* rejected *)
let n x = x [@ocaml.tailcall] (* rejected *)
let o x = (h [@tailcall]) x (* accepted *)
let p x = (h [@ocaml.tailcall]) x (* accepted *)
let q x = h x [@tailcall] (* rejected *)

module type E = sig end

module A(E:E) = struct end [@@inline] (* accepted *)
module A'(E:E) = struct end [@@ocaml.inline] (* accepted *)
module B = ((functor (E:E) -> struct end) [@inline]) (* accepted *)
module B' = ((functor (E:E) -> struct end) [@ocaml.inline]) (* accepted *)
module C = struct end [@@inline] (* rejected *)
module C' = struct end [@@ocaml.inline] (* rejected *)
module D = struct end [@@inlined] (* rejected *)
module D' = struct end [@@ocaml.inlined] (* rejected *)

module F = (A [@inlined])(struct end) (* accepted *)
module F' = (A [@ocaml.inlined])(struct end) (* accepted *)
module G = (A [@inline])(struct end) (* rejected *)
module G' = (A [@ocaml.inline])(struct end) (* rejected *)
