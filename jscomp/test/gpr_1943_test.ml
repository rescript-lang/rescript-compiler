let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites


let f x = 
  x##_003,
  x##_50,
  x##_50x, 
  x##__50, 
  x##__50x, 
  x##_50x'
  


let v = 
   f [%obj{
     _003 = 0;
     _50 = 1; _50x = 2; __50=3 ; __50x = 4;
     _50x' = 5
   }]


;; eq __LOC__ (0,1,2,3,4,5) v 
;; Mt.from_pair_suites __FILE__ !suites