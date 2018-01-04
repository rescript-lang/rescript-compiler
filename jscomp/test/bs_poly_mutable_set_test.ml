let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y 
let b loc x =  Mt.bool_suites ~test_id ~suites loc x  

module N = Bs.SetM
module I = Array_data_util
module A = Bs.Array
module IntCmp = 
  (val Bs.Cmp.make (fun[@bs] (x:int) y -> compare x y))


let () = 
  let u = N.ofArray (module IntCmp) (I.range 0 30) in 
  b __LOC__ (N.removeCheck u 0);
  b __LOC__ (not (N.removeCheck u 0));
  b __LOC__ (N.removeCheck u 30);
  b __LOC__ (N.removeCheck u 20);
  eq __LOC__ (N.length u) 28 ;
  let r = I.randomRange 0 30 in 
  b __LOC__ (Js.eqNull 29 (N.maxNull u));
  b __LOC__ (Js.eqNull 1 (N.minNull u));
  N.addOnly u 3;
  for i = 0 to A.length r - 1 do 
    N.removeOnly u (A.get r i)
  done ;
  b __LOC__ (N.isEmpty u);
  N.addOnly u 0;
  N.addOnly u 1;
  N.addOnly u 2;
  N.addOnly u 0;
  eq __LOC__ (N.length u) 3;
  b __LOC__ (not (N.isEmpty u));
  for i = 0 to 3 do 
    N.removeOnly u i 
  done ;
  b __LOC__ (N.isEmpty u);
  N.addArrayOnly u (I.randomRange 0 20000);
  N.addArrayOnly u (I.randomRange 0 200);
  eq __LOC__ (N.length u) 20001;
  N.removeArrayOnly u (I.randomRange 0 200);
  eq __LOC__ (N.length u) 19800;
  N.removeArrayOnly u (I.randomRange 0 1000);
  eq __LOC__ (N.length u) 19000;
  N.removeArrayOnly u (I.randomRange 0 1000);
  eq __LOC__ (N.length u) 19000;
  N.removeArrayOnly u (I.randomRange 1000 10000);
  eq __LOC__ (N.length u) 10000;
  N.removeArrayOnly u (I.randomRange 10000 (20000 -1));
  eq __LOC__ (N.length u) 1 ;
  b __LOC__ (N.mem u 20000)
  (* for i =  *)

;; Mt.from_pair_suites __FILE__ !suites  