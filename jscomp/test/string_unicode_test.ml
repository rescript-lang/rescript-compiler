let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites




let () = Js.log "你好"

let () = Js.log {js|你好|js}  

let () = 
    Js.log [%raw {|"你好"|}]



let f x = 
  match x with 
  | '\000' .. '\255' -> 0 
  | '\333' -> 2 
  | _ -> 3
  
let () = 
    eq __LOC__ (f '\123') 0 ;
    eq __LOC__ (f '\333') 2 ;
    eq __LOC__ (f '\444') 3
;; Mt.from_pair_suites __FILE__ !suites  