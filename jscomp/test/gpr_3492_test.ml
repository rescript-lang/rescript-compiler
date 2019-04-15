let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y 

[%%bs.raw "function foo(a){return a()}"]

external foo : ((unit -> int)[@bs.uncurry ]) -> int = ""[@@bs.val "foo"]
let fn () = 1


let () = 
   eq __LOC__ (foo fn) 1 

let () = 
  Mt.from_pair_suites __FILE__ !suites