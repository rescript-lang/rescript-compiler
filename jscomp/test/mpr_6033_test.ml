
let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites



let f x =
  match x with 
  lazy y -> y ^ "abc"

let u = 
  let x = lazy "def"   in 
  ignore (Lazy.force x );
  f x 

;; eq __LOC__ u "defabc"
;; Mt.from_pair_suites __FILE__ !suites