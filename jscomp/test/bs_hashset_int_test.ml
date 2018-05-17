let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y 
let b loc x  = Mt.bool_suites ~test_id ~suites loc x  
module N = Belt.HashSet.Int
module S = Belt.Set.Int 

module I = Array_data_util
let (++) = Belt.Array.concat
let add = fun  x y -> x + y  

let sum2 h = 
  let v = ref 0 in 
  N.forEach h (fun x -> v := !v + x) ; 
  !v 
let () = 
  let u = I.randomRange 30 100  ++ I.randomRange 40 120 in 
  let v = N.fromArray u in 
  eq __LOC__ (N.size v) 91 ;  
  let xs = S.toArray (S.fromArray (N.toArray v)) in 
  eq __LOC__ xs (I.range 30 120);
  let x =  ((30 + 120)/2  * 91) in 
  eq __LOC__ (N.reduce v 0 add) x ;
  eq __LOC__ (sum2 v ) x

let () =
  let u = I.randomRange 0 100_000 ++ I.randomRange 0 100 in 
  let v = N.make 40 in 
  N.mergeMany v u ;
  eq __LOC__ (N.size v) 100_001;
  for i = 0 to 1_000 do  
    N.remove v i 
  done ; 
  eq __LOC__ (N.size v ) 99_000;
  for i = 0 to 2_000 do  
    N.remove v i 
  done ; 
  eq __LOC__ (N.size v ) 98_000
module A = Belt.Array

module SI = Belt.SortArray.Int

let () =   
  let u0 = N.fromArray (I.randomRange 0 100_000) in 
  let u1 = N.copy u0 in 
  eq __LOC__ (N.toArray u0) (N.toArray u1);
  for i = 0 to 2000 do 
    N.remove u1 i 
  done ;  
  for i = 0 to 1000 do 
    N.remove u0 i 
  done ;
  let v0 = (A.concat (I.range 0 1000) (N.toArray u0)) in 
  let v1 = (A.concat (I.range 0 2000) (N.toArray u1)) in 
  SI.stableSortInPlace v0; 
  SI.stableSortInPlace v1;
  eq __LOC__  v0 v1 
  
let () =
  let h = N.fromArray (I.randomRange  0 1_000_000) in 
  let histo = N.getBucketHistogram h in 
  b __LOC__ (A.length histo <= 10)
let () = Mt.from_pair_suites __FILE__ !suites
