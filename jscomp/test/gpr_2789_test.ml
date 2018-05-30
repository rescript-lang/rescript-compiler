let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y 



let () = 
  eq __LOC__ 0 (Weak.length (Weak.create 0));
  eq __LOC__ 1 (Weak.length (Weak.create 1))

;; Mt.from_pair_suites __FILE__ !suites