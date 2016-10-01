

let vv = 
#if bs #then
    3
#else 
    1
#end


let v = ref 1 

let a = 
#if bs #then
 let () = incr v  in
#end !v 


let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites


let () = 
  eq __LOC__ vv 3  ;
  eq __LOC__ !v 4


;; Mt.from_pair_suites __FILE__ !suites
