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
let f x = M.ofArray (module Icmp) x 
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


;; Mt.from_pair_suites __FILE__ !suites