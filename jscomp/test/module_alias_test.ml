let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites


module N = List 

module V = Pervasives.LargeFile


module J = Js.Json

module type X = module type of List 


let f  x = 
  let module L = List in 
  Js.log x ; 
  Js.log (List.length x );
  (module L : X)

let a = 
  let h = f []   in 
  let (module L : X) = h in 
  L.length [1;2;3]

;; eq __LOC__ a 3
;; Mt.from_pair_suites __FILE__ !suites  