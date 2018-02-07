let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y 
let b loc x =  Mt.bool_suites ~test_id ~suites loc x  

module N = Bs.MutableSet
module I = Array_data_util
module A = Bs.Array
module IntCmp = 
  (val Bs.Id.comparable (fun (x:int) y -> compare x y))
module L = Bs.List
let ofArray = N.ofArray ~dict:(module IntCmp)
let empty () = N.make ~dict:(module IntCmp)


let () = 
  let u =  ofArray (I.range 0 30) in 
  b __LOC__ (N.removeCheck u 0);
  b __LOC__ (not (N.removeCheck u 0));
  b __LOC__ (N.removeCheck u 30);
  b __LOC__ (N.removeCheck u 20);
  eq __LOC__ (N.size u) 28 ;
  let r = I.randomRange 0 30 in 
  b __LOC__ (Js.eqUndefined 29 (N.maxUndefined u));
  b __LOC__ (Js.eqUndefined 1 (N.minUndefined u));
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
  b __LOC__ (N.has u 20000);
  N.removeMany u (I.randomRange 10_000 30_000);
  b __LOC__ (N.isEmpty u)


let () = 
  let v = ofArray (I.randomRange 1_000 2_000) in 
  let bs = A.map (I.randomRange 500 1499) (fun  x -> N.removeCheck v x ) in 
  let indeedRemoved = A.reduce bs 0 (fun  acc x -> if x then acc + 1 else acc) in 
  eq __LOC__ indeedRemoved 500;
  eq __LOC__ (N.size v) 501;
  let cs = A.map (I.randomRange 500 2_000) (fun  x -> N.addCheck v x) in 
  let indeedAded = A.reduce cs 0 (fun acc x -> if x then acc + 1 else acc) in 
  eq __LOC__ indeedAded 1000 ;
  eq __LOC__ (N.size v) 1_501;
  b __LOC__ (N.isEmpty (empty ()));
  eq __LOC__ (N.minimum v) (Some 500);
  eq __LOC__ (N.maximum v) (Some 2000);
  eq __LOC__ (N.minUndefined v) (Js.Undefined.return 500); 
  eq __LOC__ (N.maxUndefined v) (Js.Undefined.return 2000);
  eq __LOC__ (N.reduce v 0 (fun x y -> x + y)) ((( 500 + 2000)/2) * 1501 );
  b __LOC__ (L.eq (N.toList v) (L.makeBy 1_501 (fun i -> i + 500)  ) (fun x y -> x = y) ) ;
  eq __LOC__ (N.toArray v ) (I.range 500 2000);
  b __LOC__ (N.checkInvariantInternal v);
  eq __LOC__ (N.get v 3) None;
  eq __LOC__ (N.get v 1_200) (Some 1_200);
  let (aa, bb), pres = N.split v 1000 in 
  b __LOC__ pres ;
  b __LOC__ (A.eq (N.toArray aa) (I.range 500 999) (=));
  b __LOC__ (A.eq (N.toArray bb) (I.range 1_001 2_000) (=));
  b  __LOC__ (N.subset aa v); 
  b __LOC__ (N.subset bb v) ;
  b __LOC__ (N.isEmpty (N.intersect aa bb));
  let c = N.removeCheck v 1_000 in 
  b __LOC__ c ;
  let (aa,bb), pres = N.split v 1_000 in 
  b __LOC__ (not pres);
  b __LOC__ (A.eq (N.toArray aa) (I.range 500 999) (=));
  b __LOC__ (A.eq (N.toArray bb) (I.range 1_001 2_000) (=));
  b  __LOC__ (N.subset aa v); 
  b __LOC__ (N.subset bb v);
  b __LOC__ (N.isEmpty (N.intersect aa bb))

let (++) = N.union
let f = ofArray 
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
     =~ (empty ())
    );
  b __LOC__ 
    (N.intersect 
       (f @@ I.randomRange 21 40)
       (f @@ I.randomRange 0 20)      
     =~ (empty ())
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
  let a0 = ofArray  (I.randomRange 0 1000) in 
  let a1,a2 = 
    (
      N.keep a0 (fun x -> x mod 2  = 0),
      N.keep a0 (fun  x -> x mod 2 <> 0)
    ) in 
  let a3, a4 = N.partition a0 (fun  x -> x mod 2 = 0) in   
  b __LOC__ (N.eq a1 a3);
  b __LOC__ (N.eq a2 a4);
  b __LOC__ (L.every [a0;a1;a2;a3;a4] (fun  x -> N.checkInvariantInternal x))



;; Mt.from_pair_suites __FILE__ !suites  
