type t = [`A | `B];;
type 'a u = t;;
let a : [< int u] = `A;;

type 'a s = 'a;;
let b : [< t s] = `B;;
