let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites


let called = ref 0 
(*function hoisting prevent the toplevel bug *)
let g () = 
  let rec  v = ref next 
  and next i b = 
    incr called ; 
    if b then 
      ignore (!v i false) ; 
    i  + 1 in 


  print_endline (string_of_int  @@ next 0 true)

;; g ()

let rec x = 1::y
and y = 2::x

;; eq __LOC__ !called 2

;; Mt.from_pair_suites __FILE__ !suites