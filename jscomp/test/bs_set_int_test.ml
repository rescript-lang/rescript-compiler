let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~suites ~test_id loc x y 

let b loc v  = Mt.bool_suites ~suites ~test_id loc v 

module N = Bs.SetInt
module I = Array_data_util

let (=~) s i = 
  N.(eq (ofArray i) s)
let (=*) a b =  
  N.(eq (ofArray a) (ofArray b))
let ofA = N.ofArray

let ()= 
  b __LOC__ 
  ([|1;2;3|] =*
    [|3;2;1|])
let u = N.(inter (ofA [|1;2;3|]) (ofA [|3;4;5|]) )

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
  let l, r = N.partition v (fun[@bs] x -> x mod 3 = 0)  in 
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
  b __LOC__ (N.(inter (ofA (range 1 100)) (ofA (range 50 200))   ) =~ range 50 100)

let () = 
  b __LOC__ (N.(union (ofA (range 1 100)) (ofA (range 50 200))   ) =~ range 1 200)

let () =
    b __LOC__ (N.(diff (ofA (range 1 100)) (ofA (range 50 200))   ) =~ range 1 49)

let () = 
  b __LOC__ (N.(inter (ofA (revRange 1 100)) (ofA (revRange 50 200))   ) =~ revRange 50 100)

let () = 
  b __LOC__ (N.(union (ofA (revRange 1 100)) (ofA (revRange 50 200))   ) =~ revRange 1 200)

let () =
    b __LOC__ (N.(diff (ofA (revRange 1 100)) (ofA (revRange 50 200))   ) =~ revRange 1 49)
    
let () =     
  let ss = [|1;222;3;4;2;0;33;-1|] in 
  let v = ofA [|1;222;3;4;2;0;33;-1|] in 
  let minv, maxv = N.minNull v, N.maxNull v in 
  let approx loc (x : int)  y = 
    b loc (Js.eqNull x y) in 
  eq __LOC__ (N.fold v 0 (fun [@bs] x y -> x + y) ) (Array.fold_left (+) 0 ss) ;
  approx __LOC__ (-1) minv ;
  approx __LOC__ 222 maxv;
  let v = N.remove v 3 in 
  let minv, maxv = N.minOpt v, N.maxOpt v in 
  eq __LOC__ minv (Some (-1)); 
  eq __LOC__ maxv (Some 222);
  let v = N.remove v 222 in 
  let minv, maxv = N.minOpt v, N.maxOpt v in 
  eq __LOC__ minv (Some (-1)); 
  eq __LOC__ maxv (Some 33);
  let v = N.remove  v (-1) in 
  let minv, maxv = N.minOpt v, N.maxOpt v in 
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
  let v = Bs.Array.init count (fun [@bs] i -> i) in 
  Bs.Array.shuffleInPlace v ;
  let u = N.ofArray v in 
  b __LOC__ (N.checkInvariant u );
  let firstHalf = Bs.Array.sub v 0 2_000 in 
  let xx = Bs.Array.foldLeft firstHalf u
    (fun[@bs] acc x -> N.remove acc x ) in 
  b __LOC__ (N.checkInvariant u);
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
  b __LOC__ (not (N.subset (N.add dd 201) bb));

;; Mt.from_pair_suites __FILE__ !suites    