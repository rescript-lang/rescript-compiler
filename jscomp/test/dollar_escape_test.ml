let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites



let ($$) x y = x + y


let v = 1 $$ 2

let ($$+) x y = x * y  


let u = 1 $$+ 3 

;; eq __LOC__ v  3 
;; eq __LOC__ u 3 
;; Mt.from_pair_suites __FILE__ !suites