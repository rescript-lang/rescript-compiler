
let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites

let () = 
    eq __LOC__ (int_of_float @@ Js_float.of_any "3") 3 ;
    (* FIXME *)
     eq __LOC__ (int_of_float @@ Js_float.of_any "3.2") 3  
    (* Make sure it returns [int] *)  

let () = Mt.from_pair_suites __FILE__ !suites