
let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites




let id x = x |> Int64.to_string |> Int64.of_string 

let () = 
  let i = 0x7BABABABABABABABL in 
  let s = Int64.to_string i in   
  let i' = Int64.of_string s in 
  eq __LOC__ i i'

let () = 
    eq __LOC__ Int64.max_int (id Int64.max_int);
    eq __LOC__ Int64.min_int (id Int64.min_int)

    
let () = Mt.from_pair_suites __FILE__ !suites;;  