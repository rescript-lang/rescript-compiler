let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0

let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y 
let b = Mt.bool_suites ~test_id ~suites

let f x y = 
  Pervasives.compare (x + y) ( y + x)

let f2 x y =   
  Pervasives.compare (x + y) y

let f3 x y =   
  Pervasives.compare (x : int) y




let f4 x y = 
  min (x : int) y  

let f5_min x y =   
  min x y 
let f5_max x y = 
  max x y 

let () = 
  b __LOC__ (min 0L 1L = 0L);
  b __LOC__ (max 22L 1L = 22L);
  b __LOC__ (max (-3L) 3L = 3L);
  eq __LOC__ (f5_min None (Some 3) ) None;
  eq __LOC__ (f5_min (Some 3) None) None;
  eq __LOC__ (f5_max (Some 3) None) (Some 3);
  eq __LOC__ (f5_max None (Some 3) ) (Some 3);
  b __LOC__ (Some 5 >= None) 
  
;; Mt.from_pair_suites __FILE__ !suites  