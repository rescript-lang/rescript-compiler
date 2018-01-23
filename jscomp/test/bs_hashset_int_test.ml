let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y 

module N = Bs.HashSetInt 
module S = Bs.SetInt 

module I = Array_data_util
let (++) = Bs.Array.append 
let add = fun [@bs] x y -> x + y  

let sum2 h = 
  let v = ref 0 in 
  N.forEach h (fun [@bs] x -> v := !v + x) ; 
  !v 
let () = 
  let u = I.randomRange 30 100  ++ I.randomRange 40 120 in 
  let v = N.ofArray u in 
  eq __LOC__ (N.size v) 91 ;  
  let xs = S.toArray (S.ofArray (N.toArray v)) in 
  eq __LOC__ xs (I.range 30 120);
  let x =  ((30 + 120)/2  * 91) in 
  eq __LOC__ (N.reduce v 0 add) x ;
  eq __LOC__ (sum2 v ) x

let () =
  let u = I.randomRange 0 100_000 ++ I.randomRange 0 100 in 
  let v = N.create 40 in 
  N.mergeArrayDone v u ;
  eq __LOC__ (N.size v) 100_001;
  for i = 0 to 1_000 do  
    N.removeDone v i 
  done ; 
  eq __LOC__ (N.size v ) 99_000;
  for i = 0 to 2_000 do  
    N.removeDone v i 
  done ; 
  eq __LOC__ (N.size v ) 98_000
module A = Bs_Array
module SI = Bs_SortInt
let () =   
  let u0 = N.ofArray (I.randomRange 0 100_000) in 
  let u1 = N.copy u0 in 
  eq __LOC__ (N.toArray u0) (N.toArray u1);
  for i = 0 to 2000 do 
    N.removeDone u1 i 
  done ;  
  for i = 0 to 1000 do 
    N.removeDone u0 i 
  done ;
  let v0 = (A.append (I.range 0 1000) (N.toArray u0)) in 
  let v1 = (A.append (I.range 0 2000) (N.toArray u1)) in 
  SI.stableSort v0; 
  SI.stableSort v1;
  eq __LOC__  v0 v1 
  
  

let () = Mt.from_pair_suites __FILE__ !suites
