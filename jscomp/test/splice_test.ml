let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y 



external f : int -> int array -> int = "Math.max" 
  [@@bs.splice] [@@bs.val]

;; f 1 [||]

external send : int -> int array -> int = "" 
  [@@bs.splice] [@@bs.send]

let f00 a b =   
  a |. send [|b|]


#if 1 then
let f1 (c : int array) =  f 1 c 

;; eq __LOC__ (f1  [|2;3|] ) 3 
;; eq __LOC__ (f1  [||] ) 1
;; eq __LOC__ (f1 [|1;2;3;4;5;2;3|]) 5
#end

;; Mt.from_pair_suites __FILE__ !suites