[@@@config {
  flags = [|
   
  |]
}]

let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites




let f :  _ -> string = [%raw{| (a) => typeof a  |}] 

let a = f 3 
let b = f "3"

;; eq __LOC__ a "number"
;; eq __LOC__ b "string"
;; Mt.from_pair_suites __FILE__ !suites