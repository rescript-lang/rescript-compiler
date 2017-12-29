let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y 
let b loc x =  Mt.bool_suites ~test_id ~suites loc x  

module N  = Bs.SetIntM

module I = Array_data_util
module R = Bs.Range



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
  Js.log (N.length !v)


;; Mt.from_pair_suites __FILE__ !suites  