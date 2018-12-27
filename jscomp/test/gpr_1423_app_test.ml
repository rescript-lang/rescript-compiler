let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites




let foo f = Js.log (f ~a1:"a1" ())

let _ = foo (Gpr_1423_nav.busted ~a2:"a2")

let foo2 f = f ~a1:"a1" ()

let () = 
    eq __LOC__ 
    (foo2 (Gpr_1423_nav.busted ~a2:"a2"))
    "a1a2"

let () = Mt.from_pair_suites __MODULE__ !suites