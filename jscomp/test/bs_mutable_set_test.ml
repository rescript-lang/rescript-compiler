let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y 
let b loc x =  Mt.bool_suites ~test_id ~suites loc x  

module N  = Bs.SetIntM

module I = Array_data_util
module R = Bs.Range
module A = Bs.Array
let (++)= A.append

let () = 
  let v = ref N.empty in 
  for i = 0 to 1_00_000 do 
    (* [%assert (N.checkInvariant !v)]; *)
    v := N.add !v i 
  done ;
  b __LOC__ (N.checkInvariant !v);
  b __LOC__ @@ R.forAll 0  1_00_000 (fun [@bs] i -> 
    N.mem !v i 
   );  
  eq __LOC__ (N.length !v) 1_00_001

let () = 
  let u = I.randomRange 30 100 ++ I.randomRange 40 120 in 
  let v = ref N.empty  in 
  let v =  N.addArray !v u in  
  eq __LOC__ (N.length v) 91 ; 
  eq __LOC__ (N.toArray v) (I.range 30 120)

let () =   
  let u = I.randomRange 0 100_000 ++ I.randomRange 0 100 in 
  let v = N.ofArray u in 
  eq __LOC__ (N.length v) 100_001
  
;; Mt.from_pair_suites __FILE__ !suites  