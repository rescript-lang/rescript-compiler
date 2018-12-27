let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites



;;

let i = 
  (let e = (=) 
  (let o = false in 
  (fun z -> 
    (let m = () in fun n -> 0) 
    (fun q -> fun y -> o)) o) 0 in 0)
in  eq __LOC__ i 0 

;; Mt.from_pair_suites __MODULE__ !suites