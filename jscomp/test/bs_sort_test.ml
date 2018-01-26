let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y 
let b loc x = Mt.bool_suites ~test_id ~suites loc x 

module I = Array_data_util
module S = Bs.Sort
module R = Bs_Range
let cmp = fun[@bs] x y -> x - y   
module A = Bs.Array

let unions xs ys = 
  let lenX, lenY = A.length xs, A.length ys in 
  let o = Bs_Array.makeUninitializedUnsafe (lenX + lenY) in 
  let v = S.union xs 0 lenX ys 0 lenY o 0 cmp  in 
  A.truncateToLengthUnsafe o v ;
  o 
let inters xs ys = 
  let lenX, lenY = A.length xs, A.length ys in 
  let o = Bs_Array.makeUninitializedUnsafe lenX in 
  let v = S.intersect xs 0 lenX ys 0 lenY o 0 cmp  in 
  A.truncateToLengthUnsafe o v ;
  o 
let diffs xs ys = 
  let lenX, lenY = A.length xs, A.length ys in 
  let o = Bs_Array.makeUninitializedUnsafe lenX in 
  let v = S.diff xs 0 lenX ys 0 lenY o 0 cmp  in 
  A.truncateToLengthUnsafe o v ;
  o 

let () = 
  eq __LOC__ (unions (I.range 1 10) (I.range 3 13)) (I.range 1 13);
  eq __LOC__ (unions (I.range 1 10) (I.range 9 13)) (I.range 1 13);
  eq __LOC__ (unions (I.range 8 10) (I.range 9 13)) (I.range 8 13);
  eq __LOC__ (unions (I.range 0 2) (I.range 4 7))
  [|0;1;2;4;5;6;7|]

let () = 
  eq __LOC__ (inters (I.range 1 10) (I.range 3 13)) (I.range 3 10);
  eq __LOC__ (inters (I.range 1 10) (I.range 9 13)) (I.range 9 10);
  eq __LOC__ (inters (I.range 8 10) (I.range 9 13)) (I.range 9 10);
  eq __LOC__ (inters (I.range 0 2) (I.range 4 7)) [||]

let () = 
  eq __LOC__ (diffs (I.range 1 10) (I.range 3 13)) (I.range 1 2);
  eq __LOC__ (diffs (I.range 1 10) (I.range 9 13)) (I.range 1 8);
  eq __LOC__ (diffs (I.range 8 10) (I.range 9 13)) (I.range 8 8);
  eq __LOC__ (diffs (I.range 0 2) (I.range 4 7)) [|0;1;2|]  
let () = 
  b __LOC__ (
    R.every 0 200 (fun [@bs] i -> 
        let v = I.randomRange 0 i  in 
        S.stableSortInPlaceBy v cmp ;
        S.isSorted v cmp
      ));
  b __LOC__ (
    R.every 0 200 (fun [@bs] i -> 
        let v = I.randomRange 0 i  in 
        S.stableSortInPlaceBy v cmp ;
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


module SI = Bs.SortInt
let () =     
  let u = I.randomRange 0 1_000_000 in  
  let u1 = A.copy u in 
  let u2 = A.copy u in 
  (* let u3 = A.map u (fun[@bs] x -> float x) in  *)
  [%time S.stableSortInPlaceBy u cmp];
  b __LOC__ (S.isSorted u cmp);
  [%time SI.stableSortInPlace u2 ];
  b __LOC__ (S.isSorted u2 cmp);
  [%time S.stableSortInPlaceBy u1 cmp];
  b __LOC__ (S.isSorted u1 cmp)
  

let () = 
  let u = [|1,"a"; 1, "b"; 2, "a"|] in 
  eq __LOC__
    (S.stableSortBy 
       u
       (fun [@bs] (a,_) (b,_) -> a - b))
    [|1,"a"; 1, "b"; 2 ,"a"|];
  let u = [|1, "b"; 1,"a"; 1, "b"; 2, "a"|] in 
  eq __LOC__
    (S.stableSortBy 
       u
       (fun [@bs] (a,_) (b,_) -> a - b))
    [|1, "b";  1,"a"; 1, "b"; 2 ,"a"|] ;
  let u = [|1, "c"; 1, "b"; 1,"a"; 1, "b"; 1, "c"; 2, "a"|] in 
  eq __LOC__
    (S.stableSortBy 
       u
       (fun [@bs] (a,_) (b,_) -> a - b))
    [|1, "c"; 1, "b";  1,"a"; 1, "b"; 1, "c"; 2 ,"a"|]    



let () =     
  eq __LOC__ (S.binarySearchBy [|1;2;3;4;33;35;36|] 33 cmp ) 4 ;
  eq __LOC__ (S.binarySearchBy [|1;2;3;4;33;35;36|] 1 cmp ) 0;
  eq __LOC__ (S.binarySearchBy [|1;2;3;4;33;35;36|] 2 cmp ) 1;
  eq __LOC__ (S.binarySearchBy [|1;2;3;4;33;35;36|] 3 cmp ) 2;
  eq __LOC__ (S.binarySearchBy [|1;2;3;4;33;35;36|] 4 cmp ) 3;
  let aa = I.range 0 1000 in 
  b __LOC__ @@ R.every 0 1000 (fun [@bs] i -> 
      S.binarySearchBy aa i cmp = i 
  );
  (* 0, 2, 4, ... 4000 *)
  let cc =  A.map (I.range 0 2000 ) (fun [@bs] x -> x * 2) in 
  eq __LOC__ (lnot (S.binarySearchBy cc 5000 cmp)) (2001);
  eq __LOC__ (lnot (S.binarySearchBy cc (-1) cmp)) (0);
  eq __LOC__ (S.binarySearchBy cc 0 cmp) 0;

  eq __LOC__ ( lnot (S.binarySearchBy cc 1 cmp)) (1);
  b __LOC__ @@ R.every 0 1999 (fun [@bs] i -> 
    lnot (S.binarySearchBy cc (2 * i + 1) cmp) = (i + 1) 
    (* 1, 3, 5, ... , 3999 *)
  )

let lt = fun [@bs] (x : int) y -> x < y  
let () =   
  eq __LOC__ (S.strictlySortedLength [||] lt) 0 ;
  eq __LOC__ (S.strictlySortedLength [|1|] lt) 1;
  eq __LOC__ (S.strictlySortedLength [|1;1|] lt) 1;
  eq __LOC__ (S.strictlySortedLength [|1;1;2|] lt) 1;
  eq __LOC__ (S.strictlySortedLength [|1;2|] lt) 2;
  eq __LOC__ (S.strictlySortedLength [|1;2;3;4;3|] lt) 4;
  eq __LOC__ (S.strictlySortedLength [|4;4;3;2;1|] lt) 1;
  eq __LOC__ (S.strictlySortedLength [|4;3;2;1|] lt) (-4);
  eq __LOC__ (S.strictlySortedLength [|4;3;2;1;0|] lt) (-5);

;; Mt.from_pair_suites __FILE__ !suites  
