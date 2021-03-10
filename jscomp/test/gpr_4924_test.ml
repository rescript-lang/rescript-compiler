let {from_pair_suites;eq_suites} = (module Mt )
let suites = ref []
let id = ref 0

type t = 
  | A 
  | B 
  | C of int 
let u b = 
  match b with 
  | A -> 0 
  | B | C _ -> 1
    
let u1 b = 
  match b with 
  | A -> true 
  | B | C _ -> false   
  

let u2 b = 
  match  b with 
  | A -> false 
  | B | C _ -> true 

;; eq_suites id suites __LOC__ (u2 A) false
;; eq_suites id suites __LOC__ (u2 B ) true 
;; eq_suites id suites __LOC__ (u2 (C 2)) true
let u3 b = 
  match  b with 
  | A -> 3 
  | B | C _ -> 4

let u4 b = 
  match  b with 
  | A -> 3 
  | _ -> 4  

let u5 b = 
  match  b with 
  | A -> false
  | _ -> true

let u6 b = 
  match  b with 
  | A -> true
  | _ -> false


;; from_pair_suites __LOC__ !suites  