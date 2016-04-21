let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq f (a,b) = Mt_global.collect_eq test_id suites f  a b

let () = 
  eq __LOC__ (Scanf.sscanf "32 31" "%d %d"(fun x y -> x + y), 63)


let () =
  Mt.from_pair_suites __FILE__ !suites
