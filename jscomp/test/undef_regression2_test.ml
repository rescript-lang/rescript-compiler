
let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites


let ok loc x = 
  incr test_id; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Ok x)) :: !suites


let a = 
  match [%external ___undefined_value ] with   
  | None -> 1 
  | Some _ -> 2 

let f x = 
  Js.Undefined.test x   



let () = 
  ok __LOC__ (a > 0);
  eq __LOC__ a 1


let () = Mt.from_pair_suites __FILE__ !suites    