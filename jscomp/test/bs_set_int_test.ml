let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~suites ~test_id loc x y 

let b loc v  = Mt.bool_suites ~suites ~test_id loc v 

module N = Belt.Set.Int
module I = Array_data_util
module A = Belt.Array
let (=~) s i = 
  N.(eq (ofArray i) s)
let (=*) a b =  
  N.(eq (ofArray a) (ofArray b))
let ofA = N.ofArray

let ()= 
  b __LOC__ 
  ([|1;2;3|] =*
    [|3;2;1|])
let u = N.(intersect (ofA [|1;2;3|]) (ofA [|3;4;5|]) )

let ()= 
  b __LOC__ 
  (u
  =~ [|3|])

(* inclusive *)
let range i j = 
  Array.init (j - i + 1) (fun k -> k + i )

let revRange i j = 
    Array.init (j - i + 1) (fun k -> k + i ) |> Array.to_list |> List.rev |> Array.of_list     

let () = 
  let v = (ofA (Array.append (range 100 1000) (revRange  400 1500))) in     
  b __LOC__ (v =~ (range 100 1500));
  let l, r = N.partition v (fun x -> x mod 3 = 0)  in 
  let nl, nr = 
    let l,r = ref N.empty, ref N.empty in 
    for i = 100 to 1500 do 
      if i mod 3 = 0 then 
        l := N.add !l i
      else 
        r := N.add !r i
    done; 
    !l, !r in 
  b __LOC__ (N.eq l nl);
  b __LOC__ (N.eq r nr) 
  
let () = 
  b __LOC__ (N.(intersect (ofA (range 1 100)) (ofA (range 50 200))   ) =~ range 50 100)

let () = 
  b __LOC__ (N.(union (ofA (range 1 100)) (ofA (range 50 200))   ) =~ range 1 200)

let () =
    b __LOC__ (N.(diff (ofA (range 1 100)) (ofA (range 50 200))   ) =~ range 1 49)

let () = 
  b __LOC__ (N.(intersect (ofA (revRange 1 100)) (ofA (revRange 50 200))   ) =~ revRange 50 100)

let () = 
  b __LOC__ (N.(union (ofA (revRange 1 100)) (ofA (revRange 50 200))   ) =~ revRange 1 200)

let () =
    b __LOC__ (N.(diff (ofA (revRange 1 100)) (ofA (revRange 50 200))   ) =~ revRange 1 49)
    
let () =     
  let ss = [|1;222;3;4;2;0;33;-1|] in 
  let v = ofA [|1;222;3;4;2;0;33;-1|] in 
  let minv, maxv = N.minUndefined v, N.maxUndefined v in 
  let approx loc (x : int)  y = 
    b loc (Js.eqUndefined x y) in 
  eq __LOC__ (N.reduce v 0 (fun  x y -> x + y) ) (A.reduce ss 0 (+)  ) ;
  approx __LOC__ (-1) minv ;
  approx __LOC__ 222 maxv;
  let v = N.remove v 3 in 
  let minv, maxv = N.minimum v, N.maximum v in 
  eq __LOC__ minv (Some (-1)); 
  eq __LOC__ maxv (Some 222);
  let v = N.remove v 222 in 
  let minv, maxv = N.minimum v, N.maximum v in 
  eq __LOC__ minv (Some (-1)); 
  eq __LOC__ maxv (Some 33);
  let v = N.remove  v (-1) in 
  let minv, maxv = N.minimum v, N.maximum v in 
  eq __LOC__ minv (Some (0)); 
  eq __LOC__ maxv (Some 33);
  let v = N.remove  v 0 in 
  let v = N.remove v 33  in  
  let v = N.remove v 2 in 
  let v = N.remove v 3  in  
  let v = N.remove v 4  in 
  let v = N.remove v 1  in 
  b __LOC__ (N.isEmpty v )

 
let ()  = 
  let count = 1_000_000 in 
  let v = ((A.makeByAndShuffle count (fun i -> i))) in 
  let u = N.ofArray v in 
  (N.checkInvariantInternal u );
  let firstHalf = A.slice v 0 2_000 in 
  let xx = Belt.Array.reduce firstHalf u N.remove in 
  (N.checkInvariantInternal u);
  b __LOC__ N.(eq (union (ofArray firstHalf) xx) u)
  
let () =   
  let aa = N.ofArray (I.randomRange 0 100) in
  let bb = N.ofArray (I.randomRange 0 200) in 
  let cc = N.ofArray (I.randomRange 120 200) in 
  let dd = N.union aa cc in 
  b __LOC__ (N.subset aa bb);
  b __LOC__ (N.subset dd bb);
  b __LOC__ (N.subset (N.add dd 200) bb);
  b __LOC__ (N.add dd 200 == dd);
  b __LOC__ (N.add dd 0 == dd);
  b __LOC__ (not (N.subset (N.add dd 201) bb))


let () =   
  let aa = N.ofArray (I.randomRange 0 100) in 
  let bb = N.ofArray (I.randomRange 0 100) in 
  let cc = N.add bb 101 in 
  let dd = N.remove bb 99 in 
  let ee = N.add dd 101 in 
  b __LOC__ (N.eq aa bb );
  b __LOC__ (not (N.eq aa cc));
  b __LOC__ (not (N.eq dd cc));
  b __LOC__ (not (N.eq bb ee))

let () =   
  let a0 = N.empty in 
  let a1 = N.mergeMany a0 (I.randomRange 0 100) in 
  let a2 = N.removeMany a1 (I.randomRange 40 100) in 
  let a3 = N.ofArray (I.randomRange 0 39) in 
  let (a4,a5), pres = N.split a1 40 in 
  b __LOC__ (N.eq a1 (N.ofArray (I.randomRange 0 100)));
  b __LOC__ (N.eq a2 a3);
  b __LOC__ pres;
  b __LOC__ (N.eq a3 a4);
  let a6 = N.remove (N.removeMany a1 (I.randomRange 0 39)) 40 in 
  b __LOC__ (N.eq a5 a6);
  let a7 = N.remove a1 40 in 
  let (a8,a9), pres2 = N.split a7 40 in 
  b __LOC__ (not pres2); 
  b __LOC__ (N.eq a4 a8);
  b __LOC__ (N.eq a5 a9);
  let a10 = N.removeMany a9 (I.randomRange 42 2000)  in 
  eq __LOC__ (N.size a10 ) 1; 
  let a11 = N.removeMany a9 (I.randomRange 0 2000) in 
  b __LOC__ (N.isEmpty a11)

let () = 
  let (aa,bb), pres = N.split N.empty 0 in 
  b __LOC__ (N.isEmpty aa);
  b __LOC__ (N.isEmpty bb);
  b __LOC__ (not pres)  

let () =   
  let v = N.ofArray (I.randomRange 0 2_000) in 
  let v0 = N.ofArray (I.randomRange 0 2_000) in 
  let v1 = N.ofArray (I.randomRange 1 2_001) in 
  let v2 = N.ofArray (I.randomRange 3 2_002) in 
  let v3 = N.removeMany v2 [|2_002; 2_001|] in   
  let us = A.map (I.randomRange 1_000 3_000) (fun x -> N.has v x) in 
  let counted = A.reduce us 0 (fun  acc x -> if x then acc + 1 else acc ) in 
  eq __LOC__ counted 1_001 ; 
  b __LOC__ (N.eq v v0) ; 
  b __LOC__ (N.cmp v v0 = 0);
  b __LOC__ (N.cmp v v1 < 0); 
  b __LOC__ (N.cmp v v2 > 0);
  b __LOC__ (N.subset v3 v0);
  b __LOC__ (not (N.subset v1 v0));
  eq __LOC__ (N.get v 30 ) (Some 30);
  eq __LOC__ (N.get v 3_000 ) None
;; Mt.from_pair_suites __FILE__ !suites    
