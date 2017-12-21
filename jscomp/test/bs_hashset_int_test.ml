let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites

module N = Bs.HashSetInt 
module S = Bs.SetInt 

module I = Array_data_util
let (++) = Bs.Array.append 

let () = 
  let u = I.randomRange 30 100  ++ I.randomRange 40 120 in 
  let v = N.ofArray u in 
  eq __LOC__ (N.length v) 91 ;  
  let xs = S.toArray (S.ofArray (N.toArray v)) in 
  eq __LOC__ xs (I.range 30 120)

let () =
  let u = I.randomRange 0 100_000 ++ I.randomRange 0 100 in 
  let v = N.create 40 in 
  N.addArray v u ;
  eq __LOC__ (N.length v) 100_001;
  for i = 0 to 1_000 do  
    N.remove v i 
  done ; 
  eq __LOC__ (N.length v ) 99_000;
  for i = 0 to 2_000 do  
    N.remove v i 
  done ; 
  eq __LOC__ (N.length v ) 98_000
let () = Mt.from_pair_suites __FILE__ !suites
