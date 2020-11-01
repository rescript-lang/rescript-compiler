
let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y 



let x =  [%obj{x = 3 ; y = 4}]##x

;; [%obj{x = 3 ; y = 4}]##x |. ignore


let zz =  [%obj{_5 = 3 }]##_5




type t = {
  a : int ;  [@bs.as "0123"]
  b : int ; [@bs.as "123_456"]
} 

let h = {a = 2; b = 3 }
let f id = 
  while false do () done;  
  id 

  
;; eq __LOC__ [%obj{_5 = 3 }]##_5 3 
;; eq __LOC__ (2,3) ((f h).a,(f h).b)
;; Js.log @@ Obj.tag (Obj.repr [%obj{_5 = 3 }])

;; Mt.from_pair_suites __LOC__ !suites