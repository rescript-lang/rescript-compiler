
[@@@bs.config {flags= [|"-bs-diagnose"|]}]

let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eqs loc x y = Mt.eq_suites ~test_id ~suites loc x y 


let eq =fun [@bs] {contents = (x:int)} {contents = y} ->  x = y

let eq2 =fun [@bs] x {contents = y} ->  x.contents = y

;; eqs __LOC__ false (eq (ref 1) (ref 2) [@bs])
;; eqs __LOC__ true (eq (ref 2) (ref 2) [@bs])

let u = object 
  method hi {contents = x} {contents = y} =
    (x : int) = y
end;;


let h = u##hi (ref 1) (ref 2)

;; eqs __LOC__ h false


let ut3 = fun [@bs] {contents = x0} {contents = x1} {contents = x2} -> 
  (x0,x1,x2)
let t3 = fun {contents = x0} {contents = x1} {contents = x2} -> 
  (x0,x1,x2)

let ut4 = fun [@bs] {contents = x0} {contents = x1} {contents = x2} {contents = x3} -> 
  (x0,x1,x2,x3)

let ut5 = fun [@bs] {contents = x0} {contents = x1} {contents = x2} {contents = x3} {contents = x4}-> 
  (x0,x1,x2,x3,x4)

  ;; eqs __LOC__ (ut3 (ref 1) (ref 2) (ref 3) [@bs]) (1,2,3) 
;; eqs __LOC__ (t3 (ref 1) (ref 2) (ref 3) ) (1,2,3) 

;; eqs __LOC__ (ut5 (ref 1) (ref 2) (ref 3) (ref 1) (ref 1)[@bs]) (1,2,3,1,1) 
;; Mt.from_pair_suites __FILE__ !suites