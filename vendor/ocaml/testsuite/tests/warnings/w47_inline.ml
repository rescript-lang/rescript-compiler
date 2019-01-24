
let a = (fun x -> x) [@inline] (* accepted *)
let b = (fun x -> x) [@inline never] (* accepted *)
let c = (fun x -> x) [@inline always] (* accepted *)
let d = (fun x -> x) [@inline malformed attribute] (* rejected *)
let e = (fun x -> x) [@inline malformed_attribute] (* rejected *)
let f = (fun x -> x) [@inline : malformed_attribute] (* rejected *)
let g = (fun x -> x) [@inline ? malformed_attribute] (* rejected *)

let h x = (a [@inlined]) x (* accepted *)
let i x = (a [@inlined never]) x (* accepted *)
let j x = (a [@inlined always]) x (* accepted *)
let k x = (a [@inlined malformed]) x (* rejected *)

let l x = x [@@inline] (* accepted *)
