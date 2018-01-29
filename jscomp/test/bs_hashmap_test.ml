let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eqx loc x y = Mt.eq_suites ~test_id ~suites loc x y 
let b loc x = Mt.bool_suites ~test_id ~suites loc x 
module N = Bs.UnorderedMutableMap
module S = Bs.Map.Int
(* module Y = struct  
   type t = int 

   end *)
let eq = fun[@bs] (x : int) y ->  x = y 
let hash = fun[@bs] (x : int) ->  Hashtbl.hash x 
let cmp = fun [@bs] (x : int) y -> compare x y
module Y = (val Bs.Hash.make ~eq ~hash)
let empty : (int, int, _) N.t = N.make ~dict:(module Y) 30 

(*
[%bs.hash {
  eq : 
  hash : 
}]
*)

module I = Array_data_util
let (++) = Bs.Array.concat
let add = fun [@bs] x y -> x + y  


let () = 
  N.mergeMany empty [|1,1;2,3;3,3; 2,2|];
  eqx __LOC__ (N.get empty 2) (Some 2);
  eqx __LOC__ (N.size empty) 3
  
module A = Bs.Array 
module So = Bs.SortArray 

let () = 
  let u = I.randomRange 30 100 ++ I.randomRange 40 120 in 
  let v = A.zip u u in 
  let xx = N.ofArray ~dict:(module Y) v  in 
  eqx __LOC__ (N.size xx) 91;
  eqx __LOC__ (So.stableSortBy (N.keysToArray xx) cmp) (I.range 30 120)

let () = 
  let u = I.randomRange 0 100_000 ++ I.randomRange 0 100 in 
  let v = N.make ~dict:(module Y) 40 in 
  N.mergeMany v (A.zip u u);
  eqx __LOC__ (N.size v) 100_001;
  for i = 0 to 1_000 do 
    N.remove v i 
  done; 
  eqx __LOC__ (N.size v) 99_000;
  for i = 0 to 2_000 do 
    N.remove v i 
  done ;
  eqx __LOC__ (N.size v) 98_000;
  b __LOC__ (A.every (I.range 2_001 100_000) (fun [@bs] x -> N.has v x ))

;; Mt.from_pair_suites __FILE__ !suites