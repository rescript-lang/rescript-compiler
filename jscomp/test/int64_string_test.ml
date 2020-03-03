
let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y 


let v = Int64.to_string 333L

let f a b =
  eq __LOC__ (Int64.to_string a) b 


;; f (-33L) "-33"  
;; f (33L) "33"
;; f Int64.min_int "-9223372036854775808"
;; f (Int64.(add min_int 100L))
  "-9223372036854775708"
;; Mt.from_pair_suites __LOC__ !suites