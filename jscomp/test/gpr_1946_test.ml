
let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y 



let x =  [%obj{x = 3 ; y = 4}]##x

;; [%obj{x = 3 ; y = 4}]##x


let zz =  [%obj{_5 = 3 }]##_5



;; eq __LOC__ [%obj{_5 = 3 }]##_5 3 
;; Js.log @@ Obj.tag (Obj.repr [%obj{_5 = 3 }])

;; Mt.from_pair_suites __LOC__ !suites