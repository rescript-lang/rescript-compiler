let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y 

module H = Inline_const.N1 (struct end)

let f , 
    f1 ,
    f2,
    f3,
    f4
    = 

  Inline_const.(
    f ,
    f1 ,
    f2 ,
    N.f3,
    H.f4
  )

let () =   
  eq __LOC__ f "hello";
  eq __LOC__ f1 "a";
  eq __LOC__ f2 {j|中文|j};
  eq __LOC__ f3 {j|中文|j};
  eq __LOC__ f4 {j|中文|j}

let () =   
  Mt.from_pair_suites __LOC__ !suites