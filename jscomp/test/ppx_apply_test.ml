let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites


let u = (fun [@fn] a  b -> a + b )  1  2  [@fn]

let nullary = fun [@fn] () -> 3 

let unary = fun [@fn] a -> a + 3 

let xx = unary  3 [@fn]
let () = 
  eq __LOC__ u 3 

;;

Mt.from_pair_suites __FILE__ !suites
