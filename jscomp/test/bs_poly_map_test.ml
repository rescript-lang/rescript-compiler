let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~suites ~test_id loc x y 
let b loc v  = Mt.bool_suites ~suites ~test_id loc v 

module Icmp = 
  (val Bs.Cmp.make 
      (fun[@bs] (x : int) y -> 
         compare x y
      )
  )
module M = Bs.Map  
module N = Bs.Set 

module A = Bs_Array
module I = Array_data_util
let f x = M.ofArray ~dict:(module Icmp) x 
let ff x = N.ofArray (module Icmp) x 

let mergeInter s1 s2 = 
  ff @@ M.keysToArray (M.merge s1 s2 (fun[@bs] k v1 v2 -> 
      match v1,v2 with 
      | Some _, Some _ -> Some ()
      | _, _ -> None
    ))

let mergeUnion s1 s2 =    
  ff @@ M.keysToArray @@ M.merge s1 s2 (fun[@bs] k v1 v2 -> 
      match v1,v2 with 
      | None, None -> None
      | _, _ -> Some ()
    )
let mergeDiff s1 s2 =    
  ff @@ M.keysToArray @@ M.merge s1 s2 (fun[@bs] k v1 v2 -> 
      match v1,v2 with 
      | Some _, None -> Some ()
      | Some _, Some _
      | None, _ -> None   
    )

let randomRange i j = 
  A.map (I.randomRange i j) (fun[@bs] x -> (x,x))

let () =    
  let u0 = f (randomRange 0 100) in 
  let u1 = f (randomRange 30 120) in 
  b __LOC__ (N.eq (mergeInter u0 u1) (ff (I.range 30 100)));
  b __LOC__ (N.eq (mergeUnion u0 u1) (ff (I.range 0 120)));
  b __LOC__ (N.eq (mergeDiff u0 u1) (ff (I.range 0 29)));
  b __LOC__ (N.eq (mergeDiff u1 u0) (ff (I.range 101 120)))


let () =   
  let a0 = f (randomRange 0 10) in 
  let a1 = M.set a0 3 33 in (* (3,3) *)
  let a2 = M.remove a1 3 in  (* no 3 *)
  let a3 = M.update a2 3 (fun[@bs]  k -> 
      match k with 
      | Some k -> Some (k + 1)
      | None  ->  Some 11
    ) in  (* 3, 11 *)
  let a4 = M.update a2 3 (fun[@bs]  k -> 
      match k with 
      | Some k-> Some (k + 1)
      | None  ->  None
    ) in  (* no 3 *)
  let a5 = M.remove a0 3 in   
  let a6 = M.remove a5 3 in 
  b __LOC__ (a5 == a6);
  b __LOC__ (M.has a0 3);
  b __LOC__ (not (M.has a5 3));
  b __LOC__ (Js.eqNull 3 (M.getNull a0 3));
  b __LOC__ (Js.eqNull 33 (M.getNull a1 3));
  b __LOC__ (Js.Null.test (M.getNull a2 3));

  b __LOC__ (Js.eqNull 11 (M.getNull a3 3));
  b __LOC__ (Js.Null.test (M.getNull a4 3));

  let a7 = M.removeArray a0 [|7;8;0;1;3;2;4;922;4;5;6;|] in 
  eq __LOC__ (M.keysToArray a7) [|9;10|];
  let a8 = M.removeArray a7 (I.randomRange 0 100) in 
  b __LOC__ (M.isEmpty a8)

(* TODO: expose [Bs_Bag.bag] makes the error message
  pretty hard to read
  {[
    Error: This expression has type
         ((Icmp.t, Icmp.id) Bs_Cmp.t, (Icmp.t, Icmp.t, Icmp.id) Bs_Map.t0)
         Bs_Bag.bag
       but an expression was expected of type unit
  ]}
 *)  
let () =   
  let module Array = M in 
  let u0 = f (randomRange 0 100) in 
  let u1 = u0.(3) <- 32  in 
  eq __LOC__ u1.(3) (Some 32); 
  eq __LOC__ u0.(3) (Some 3)


let acc m i =   
  M.update m i (fun[@bs] n -> match n with None -> Some 1 | Some acc -> Some (acc + 1))

let acc m is : _ M.t =   
  A.foldLeft is m (fun[@bs] a i -> acc a i) 

let () = 
  let m = M.empty (module Icmp) in 
  let m1 = acc m (A.append (I.randomRange 0 20) (I.randomRange 10 30)) in 
  b __LOC__ 
  (M.eq m1 
  (M.ofArray ~dict:(module Icmp) (A.init 31 (fun[@bs] i -> i, if i >= 10 && i <= 20 then 2 else 1 )))
  (fun[@bs] x y -> x = y)
  )

;; Mt.from_pair_suites __FILE__ !suites