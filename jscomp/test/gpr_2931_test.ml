
let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y 



let fake_c2 a_type b_type  = 
  match a_type, b_type with 
  | "undefined", _ -> - 1
  | _, "undefined" -> 1 
  | "string", _ -> 1 
  | "number", "number" -> 
      33
  | "number", _ -> 3 
  | _ ->        
        0

;; eq __LOC__ 3 (fake_c2 "number" "xx")
;; Mt.from_pair_suites __FILE__ !suites        
