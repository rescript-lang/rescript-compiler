

let {from_pair_suites;eq_suites} = (module Mt )

let suites = ref []
let test_id = ref 0
type show = | No | After of int  | Yes 


let showToJs x = 
  match x with   
  | Yes | After _ -> true 
  | No -> false

;; eq_suites ~test_id ~suites __LOC__ (showToJs Yes) true 
;; eq_suites ~test_id ~suites __LOC__ (showToJs No) false 
;; eq_suites ~test_id ~suites __LOC__ (showToJs (After 3)) true

;; from_pair_suites __LOC__ !suites