
let suites =  ref []
let test_id = ref 0 
let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y
let b loc x = Mt.bool_suites ~test_id ~suites loc x

module Block = struct 
end 
type t = 
  | A0 of int
  | A of int * int 

let v0 = A (0,1)


module N = struct 
  module Block = struct end
  let v1 = A (0,1) 
end 

module Caml_obj = struct 
end 

module V = struct 
  module List = struct 
  end 
end 
let f a b = a = b

let h = List.length 

;; eq __LOC__ (h [1;2]) 2 
;; b __LOC__ (f v0 (A(0,1))) 
;; eq __LOC__ v0 N.v1

;; Mt.from_pair_suites __FILE__ !suites