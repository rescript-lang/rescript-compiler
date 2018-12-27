let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites



let () =
  let number = 1_000_00 in 
  let s = ref String_set.empty in 
  for i = 0 to  number - 1  do
    s := String_set.add  (string_of_int i ) !s
  done ;
  eq __LOC__ (String_set.cardinal !s ) number 

let () = Mt.from_pair_suites __MODULE__ !suites
