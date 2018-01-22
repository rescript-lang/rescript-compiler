let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eqx loc x y = Mt.eq_suites ~test_id ~suites loc x y 
let b loc x = Mt.bool_suites ~test_id ~suites loc x 
module N = Bs.HashMap
module S = Bs.MapInt
(* module Y = struct  
   type t = int 

   end *)
let eq = fun[@bs] (x : int) y ->  x = y 
let hash = fun[@bs] (x : int) ->  Hashtbl.hash x 
let cmp = fun [@bs] (x : int) y -> compare x y
module Y = (val Bs.Hash.make ~eq ~hash)
let empty : (int, int, _) N.t = N.create (module Y) 30 

(*
[%bs.hash {
  eq : 
  hash : 
}]
*)

module I = Array_data_util
let (++) = Bs.Array.append 
let add = fun [@bs] x y -> x + y  


let () = 
  N.mergeArrayDone empty [|1,1;2,3;3,3; 2,2|];
  eqx __LOC__ (N.get empty 2) (Some 2);
  eqx __LOC__ (N.size empty) 3
  
module A = Bs.Array 
module So = Bs.Sort 
let () = 
  let u = I.randomRange 30 100 ++ I.randomRange 40 120 in 
  let v = A.zip u u in 
  let xx = N.ofArray (module Y) v  in 
  eqx __LOC__ (N.size xx) 91;
  eqx __LOC__ (So.sortByCont (N.keys xx) cmp) (I.range 30 120)

let () = 
  let u = I.randomRange 0 100_000 ++ I.randomRange 0 100 in 
  let v = N.create (module Y) 40 in 
  N.mergeArrayDone v (A.zip u u);
  eqx __LOC__ (N.size v) 100_001;
  for i = 0 to 1_000 do 
    N.remove v i 
  done; 
  eqx __LOC__ (N.size v) 99_000;
  for i = 0 to 2_000 do 
    N.remove v i 
  done ;
  eqx __LOC__ (N.size v) 98_000;
  b __LOC__ (A.forAll (I.range 2_001 100_000) (fun [@bs] x -> N.has v x ))

;; Mt.from_pair_suites __FILE__ !suites