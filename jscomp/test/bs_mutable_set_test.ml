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
  let v = N.empty () in 
  for i = 0 to 1_00_000 do 
    (* [%assert (N.checkInvariant !v)]; *)
     N.addOnly v i 
  done ;
  b __LOC__ (N.checkInvariant v);
  b __LOC__ @@ R.forAll 0  1_00_000 (fun [@bs] i -> 
    N.mem v i 
   );  
  eq __LOC__ (N.length v) 1_00_001

let () = 
  let u = I.randomRange 30 100 ++ I.randomRange 40 120 in 
  let v = N.empty () in 
  N.addArrayOnly v u ;
  eq __LOC__ (N.length v) 91 ; 
  eq __LOC__ (N.toArray v) (I.range 30 120)

let () =   
  let u = I.randomRange 0 100_000 ++ I.randomRange 0 100 in 
  let v = N.ofArray u in 
  eq __LOC__ (N.length v) 100_001;
  let u = I.randomRange 50_000 80_000 in 
  
  for i = 0 to A.length u - 1 do 
    N.removeOnly v i 
  done;
  
  eq __LOC__ (N.length v) 70_000;
  let count = 100_000 in 
  let vv = I.randomRange 0 count in 
  for i  = 0 to A.length vv - 1 do 
    N.removeOnly v vv.(i)
  done; 
  eq __LOC__ (N.length v) 0;
  b __LOC__ (N.isEmpty v )

let () = 
  let v = N.ofArray (A.init 30 (fun [@bs]i -> i)) in 
  N.removeOnly v 30; 
  N.removeOnly v 29 ;
  b __LOC__ (Js.eqNull 28 (N.maxNull v ));
  N.removeOnly v 0 ; 
  b __LOC__ (Js.eqNull 1 (N.minNull v));
  eq __LOC__ (N.length v ) 28;
  let vv = I.randomRange 1 28 in 
  for i = 0 to A.length vv - 1 do  
    N.removeOnly v vv.(i)
  done  ;
  eq __LOC__ (N.length v) 0 

let () =   
  let id loc x = 
    let u = (N.ofSortedArrayUnsafe x) in
    b loc (N.checkInvariant u );
    b loc (A.forAll2 (N.toArray u) x (fun[@bs] x y -> x = y) )
  in 
  id __LOC__ [||] ; 
  id __LOC__ [|0|];
  id __LOC__ [|0;1|];
  id __LOC__ [|0;1;2|];
  id __LOC__ [|0;1;2;3|];
  id __LOC__ [|0;1;2;3;4|];
  id __LOC__ [|0;1;2;3;4;5|];
  id __LOC__ [|0;1;2;3;4;6|];
  id __LOC__ [|0;1;2;3;4;6;7|];
  id __LOC__ [|0;1;2;3;4;6;7;8|];
  id __LOC__ [|0;1;2;3;4;6;7;8;9|];
  id __LOC__ (I.range 0 1000)
  
let () =   
  let v = N.ofArray (I.randomRange 0 1000) in 
  let copyV = N.filter v (fun[@bs] x -> x mod 8 = 0) in 
  let aa,bb = N.partition v (fun[@bs] x -> x mod 8 = 0) in 
  let cc = N.filter v (fun[@bs] x -> x mod 8 <> 0) in 
  for i = 0 to 200 do 
    N.removeOnly v i
  done ;
  eq __LOC__ (N.length copyV) 126; 
  eq __LOC__ (N.toArray copyV) (A.init 126 (fun[@bs] i -> i * 8));
  eq __LOC__ (N.length v ) 800;
  b __LOC__ (N.eq copyV aa);
  b __LOC__ (N.eq cc bb)

;; Mt.from_pair_suites __FILE__ !suites  