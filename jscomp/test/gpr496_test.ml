let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites


let expected = 
  (true = false,
   false = true, 
   false = false, 
   true = true,
   Pervasives.compare false true , 
   Pervasives.compare true false,
   Pervasives.compare false false,
   Pervasives.compare true true
  ) 

let u = 
  (Js.true_ = Js.false_,
   Js.false_ = Js.true_, 
   Js.false_ = Js.false_, 
   Js.true_ = Js.true_,
   Pervasives.compare Js.false_ Js.true_ , 
   Pervasives.compare Js.true_ Js.false_,
   Pervasives.compare Js.false_ Js.false_,
   Pervasives.compare Js.true_ Js.true_
  ) 


let () = eq __LOC__ expected u 

(* let () = Js.log (expected, u) *)



let () = Mt.from_pair_suites __FILE__ !suites
