let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y 
let b loc x =  Mt.bool_suites ~test_id ~suites loc x  

module N = Bs.SetM
module I = Array_data_util
module A = Bs.Array
module IntCmp = 
  (val Bs.Cmp.make (fun[@bs] (x:int) y -> compare x y))
module L = Bs.List

let () = 
  let u = N.ofArray ~dict:(module IntCmp) (I.range 0 30) in 
  b __LOC__ (N.removeCheck u 0);
  b __LOC__ (not (N.removeCheck u 0));
  b __LOC__ (N.removeCheck u 30);
  b __LOC__ (N.removeCheck u 20);
  eq __LOC__ (N.size u) 28 ;
  let r = I.randomRange 0 30 in 
  b __LOC__ (Js.eqNull 29 (N.maxNull u));
  b __LOC__ (Js.eqNull 1 (N.minNull u));
  N.add u 3;  
  for i = 0 to A.length r - 1 do 
    N.remove u (A.getUnsafe r i)
  done ;
  b __LOC__ (N.isEmpty u);
  N.add u 0;
  N.add u 1;
  N.add u 2;
  N.add u 0;
  eq __LOC__ (N.size u) 3;
  b __LOC__ (not (N.isEmpty u));
  for i = 0 to 3 do 
    N.remove u i 
  done ;
  b __LOC__ (N.isEmpty u);
  N.mergeMany u (I.randomRange 0 20000);
  N.mergeMany u (I.randomRange 0 200);
  eq __LOC__ (N.size u) 20001;
  N.removeMany u (I.randomRange 0 200);
  eq __LOC__ (N.size u) 19800;
  N.removeMany u (I.randomRange 0 1000);
  eq __LOC__ (N.size u) 19000;
  N.removeMany u (I.randomRange 0 1000);
  eq __LOC__ (N.size u) 19000;
  N.removeMany u (I.randomRange 1000 10000);
  eq __LOC__ (N.size u) 10000;
  N.removeMany u (I.randomRange 10000 (20000 -1));
  eq __LOC__ (N.size u) 1 ;
  b __LOC__ (N.has u 20000)
(* for i =  *)
let (++) = N.union
let f = N.ofArray ~dict:(module IntCmp) 
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
  let dd = N.intersect aa bb in 
  b __LOC__ (dd =~ f (I.randomRange 40 100));
  b __LOC__ 
    (N.intersect 
       (f @@ I.randomRange 0 20)
       (f @@ I.randomRange 21 40)
     =~ (N.empty (module IntCmp))
    );
  b __LOC__ 
    (N.intersect 
       (f @@ I.randomRange 21 40)
       (f @@ I.randomRange 0 20)      
     =~ (N.empty (module IntCmp))
    );  
  b __LOC__  
    (N.intersect 
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
    )     

let () =   
  let a0 = N.ofArray ~dict:(module IntCmp) (I.randomRange 0 1000) in 
  let a1,a2 = 
    (
      N.keepBy a0 (fun [@bs] x -> x mod 2  = 0),
      N.keepBy a0 (fun [@bs] x -> x mod 2 <> 0)
    ) in 
  let a3, a4 = N.partition a0 (fun [@bs] x -> x mod 2 = 0) in   
  b __LOC__ (N.eq a1 a3);
  b __LOC__ (N.eq a2 a4);
  b __LOC__ (L.every [a0;a1;a2;a3;a4] (fun [@bs] x -> N.checkInvariant x))



;; Mt.from_pair_suites __FILE__ !suites  