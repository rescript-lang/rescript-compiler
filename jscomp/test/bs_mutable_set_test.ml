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
     N.addDone v i 
  done ;
  b __LOC__ (N.checkInvariant v);
  b __LOC__ @@ R.forAll 0  1_00_000 (fun [@bs] i -> 
    N.has v i 
   );  
  eq __LOC__ (N.size v) 1_00_001

let () = 
  let u = I.randomRange 30 100 ++ I.randomRange 40 120 in 
  let v = N.empty () in 
  N.addArrayDone v u ;
  eq __LOC__ (N.size v) 91 ; 
  eq __LOC__ (N.toArray v) (I.range 30 120)

let () =   
  let u = I.randomRange 0 100_000 ++ I.randomRange 0 100 in 
  let v = N.ofArray u in 
  eq __LOC__ (N.size v) 100_001;
  let u = I.randomRange 50_000 80_000 in 
  
  for i = 0 to A.length u - 1 do 
    N.removeDone v i 
  done;
  
  eq __LOC__ (N.size v) 70_000;
  let count = 100_000 in 
  let vv = I.randomRange 0 count in 
  for i  = 0 to A.length vv - 1 do 
    N.removeDone v vv.(i)
  done; 
  eq __LOC__ (N.size v) 0;
  b __LOC__ (N.isEmpty v )

let () = 
  let v = N.ofArray (A.init 30 (fun [@bs]i -> i)) in 
  N.removeDone v 30; 
  N.removeDone v 29 ;
  b __LOC__ (Js.eqNull 28 (N.maxNull v ));
  N.removeDone v 0 ; 
  b __LOC__ (Js.eqNull 1 (N.minNull v));
  eq __LOC__ (N.size v ) 28;
  let vv = I.randomRange 1 28 in 
  for i = 0 to A.length vv - 1 do  
    N.removeDone v vv.(i)
  done  ;
  eq __LOC__ (N.size v) 0 

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
    N.removeDone v i
  done ;
  eq __LOC__ (N.size copyV) 126; 
  eq __LOC__ (N.toArray copyV) (A.init 126 (fun[@bs] i -> i * 8));
  eq __LOC__ (N.size v ) 800;
  b __LOC__ (N.eq copyV aa);
  b __LOC__ (N.eq cc bb)

let () =   
  let v = N.ofArray (I.randomRange 0 1000) in 
  let ((aa,bb),_) = N.split v 400 in 
  b __LOC__ (N.eq aa (N.ofArray (I.randomRange 0 399))) ;
  b __LOC__ (N.eq bb (N.ofArray (I.randomRange 401 1000)));
  let d = N.ofArray (A.map (I.randomRange 0 1000) (fun[@bs] x -> x * 2)) in 
  let ((cc,dd), _) = N.split d 1001 in   
  b __LOC__ (N.eq cc (N.ofArray (A.init 501 (fun[@bs] x -> x * 2))));
  b __LOC__ (N.eq dd (N.ofArray (A.init 500 (fun [@bs] x -> 1002 + x * 2))))


let (++) = N.union
let f = N.ofArray 
let (=~) = N.eq 
let () =   
  let aa =  f (I.randomRange 0 100) in 
  let bb = f  (I.randomRange 40 120) in
  let cc = aa ++ bb in 
  b __LOC__ (cc =~ f (I.randomRange 0 120));

  b __LOC__ (N.eq 
               ( N.union (f (I.randomRange 0 20))
                   (f (I.randomRange 21 40) ))
               (f( I.randomRange 0 40)));
  let dd = N.inter aa bb in 
  b __LOC__ (dd =~ f (I.randomRange 40 100));
  b __LOC__ 
    (N.inter 
       (f @@ I.randomRange 0 20)
       (f @@ I.randomRange 21 40)
     =~ (N.empty ())
    );
  b __LOC__ 
    (N.inter 
       (f @@ I.randomRange 21 40)
       (f @@ I.randomRange 0 20)      
     =~ (N.empty ())
    );  
  b __LOC__  
    (N.inter 
       (f [|1;3;4;5;7;9|])
       (f [|2;4;5;6;8;10|])
     =~ (f [|4;5|])  
    );
  b __LOC__
    (N.diff aa bb =~ f (I.randomRange 0 39));
  b __LOC__   
    (N.diff bb aa =~ f (I.randomRange 101 120));
  b __LOC__ 
    (N.diff
       (f @@ I.randomRange 21 40)
       (f @@ I.randomRange 0 20)      
     =~ (f (I.randomRange 21 40))
    );  
  b __LOC__ 
    (N.diff
       (f @@ I.randomRange 0 20)
       (f @@ I.randomRange 21 40)      
     =~ (f (I.randomRange 0 20))
    );    

  b __LOC__ 
    (N.diff
       (f @@ I.randomRange 0 20)
       (f @@ I.randomRange 0 40)      
     =~ (f (I.randomRange 0 (-1)))
    );      

  

;; Mt.from_pair_suites __FILE__ !suites  

