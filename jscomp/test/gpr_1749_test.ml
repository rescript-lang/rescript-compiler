
let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites



let a =
     if 1. < (match 1. < 1. with | true  -> 1. | false  -> 10.)
     then 0
     else 1



;; eq __LOC__ 0 a
;; Mt.from_pair_suites __FILE__ !suites     

