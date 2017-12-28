let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y 
let b loc x = Mt.bool_suites ~test_id ~suites loc x 

module I = Array_data_util
module S = Bs.Sort
module R = Bs_Range
let cmp = fun[@bs] x y -> x - y   
let () = 
  b __LOC__ (
    R.forAll 0 200 (fun [@bs] i -> 
        let v = I.randomRange 0 i  in 
        S.stableSortBy v cmp ;
        S.isSorted v cmp
      ));
  b __LOC__ (
    R.forAll 0 200 (fun [@bs] i -> 
        let v = I.randomRange 0 i  in 
        S.sortBy v cmp ;
        S.isSorted v cmp
      ));
  b __LOC__     
    (S.isSorted [||] cmp);

  b __LOC__     
    (S.isSorted [|0|] cmp);

  b __LOC__     
    (S.isSorted [|0;1|] cmp);    
  b __LOC__     
    (not @@ S.isSorted [|1;0|] cmp)        

module A = Bs.Array
let () =     
  let u = I.randomRange 0 1_000_000 in  
  let u1 = A.copy u in 
  let u2 = A.copy u in 
  let u3 = A.map u (fun[@bs] x -> float x) in 
  [%time S.stableSortBy u cmp];
  b __LOC__ (S.isSorted u cmp);
  [%time S.stableSortInts u2 ];
  b __LOC__ (S.isSorted u2 cmp);
  [%time S.sortBy u1 cmp];
  b __LOC__ (S.isSorted u1 cmp)
  

let () = 
  let u = [|1,"a"; 1, "b"; 2, "a"|] in 
  eq __LOC__
    (S.stableSortBy 
       u
       (fun [@bs] (a,_) (b,_) -> a - b); u)
    [|1,"a"; 1, "b"; 2 ,"a"|];
  let u = [|1, "b"; 1,"a"; 1, "b"; 2, "a"|] in 
  eq __LOC__
    (S.stableSortBy 
       u
       (fun [@bs] (a,_) (b,_) -> a - b); u)
    [|1, "b";  1,"a"; 1, "b"; 2 ,"a"|] ;
  let u = [|1, "c"; 1, "b"; 1,"a"; 1, "b"; 1, "c"; 2, "a"|] in 
  eq __LOC__
    (S.stableSortBy 
       u
       (fun [@bs] (a,_) (b,_) -> a - b); u)
    [|1, "c"; 1, "b";  1,"a"; 1, "b"; 1, "c"; 2 ,"a"|]    


;; Mt.from_pair_suites __FILE__ !suites  