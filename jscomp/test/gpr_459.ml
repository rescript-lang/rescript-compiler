let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites



let uu = {
  _'x_ = 3
} [@bs.obj]


let hh = uu##_'x_

let () = eq __LOC__ hh 3 

let () = Mt.from_pair_suites __FILE__ !suites
