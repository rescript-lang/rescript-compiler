let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y 

module N = Bs.HashMapInt
module S = Bs.SetInt 

module I = Array_data_util
let (++) = Bs.Array.append 
let add = fun [@bs] x y -> x + y  


  


;; Mt.from_pair_suites __FILE__ !suites