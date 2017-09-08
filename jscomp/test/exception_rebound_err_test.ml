let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites

exception A of int 
exception B 
exception C of int * int 

let test_js_error4 () = 
  try ignore @@ Js.Json.parseExn {| {"x"}|}; 1 with
  | Not_found -> 2
  | Invalid_argument "x" -> 3 
  | A 2 -> 4
  | B -> 5  
  | C (1,2) -> 6
  | e -> 7

let f g = 
  try g () with Not_found -> 1 

;; eq __LOC__ (test_js_error4 ()  ) 7
;; Mt.from_pair_suites __FILE__ !suites  