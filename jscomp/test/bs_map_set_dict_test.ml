let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~suites ~test_id loc x y 
let b loc v  = Mt.bool_suites ~suites ~test_id loc v 

module Icmp = 
  (val Belt.Id.comparable
    ~cmp:(fun (x : int) y -> 
      compare x y
     )
  )
module Icmp2 = 
(val Belt.Id.comparable ~cmp:(fun  (x : int) y ->
      compare x y ))
  
module M = Belt.Map
module MI = Belt.Map.Int
(* module B = Belt.Bag *)
module I = Array_data_util
module A = Bs_Array
module L = Belt.List
let m0 : (_,string,_) M.t = M.make (module Icmp)   

  
module I2 = 
(val Belt.Id.comparable ~cmp:(fun  (x : int) y -> compare y x ))

  
let m = M.make (module Icmp2)
let m2 : (int, string, _) M.t = M.make (module I2)
let vv = MI.empty 
let vv2 = MI.empty
module Md0 = Belt.Map.Dict
let () = 
  let count = 1_000_00 in 
  let data = ref (M.getData m) in 
  let m2_dict, m_dict = M.(getId m2, getId m) in 
  let module N = (val m2_dict) in 
  let module Mm = ( val m_dict) in
  for i = 0 to count do 
    data := 
      Md0.set !data 
      ~cmp:  Mm.cmp
      i i 
  done ;
  let newm = M.packIdData ~data:!data ~id:m_dict in 
  Js.log newm
module ISet = Belt.Set 
let () =     
  let  m = Md0.empty in 
  let m11 = 
    Md0.set ~cmp:Icmp.cmp m
    1 1 
  in  
  let _m20 = M.make (module Icmp) in 
  Js.log m11

module S0 = Belt.Set.Dict  
let () =   
 let count = 100_000 in 
  let v = ISet.make (module Icmp2) in 
  let m_dict = M.getId m in 
  let module M = (val m_dict) in 
  let cmp = M.cmp in 
  let data = ref (ISet.getData v) in 
  for i = 0 to count do 
    data := S0.add ~cmp !data i 
  done ;
  Js.log !data  

let f = M.ofArray ~id:(module Icmp)
let (=~) a b = M.eq a b  

let () =   
  let u0 =  f (A.map (I.randomRange 0 39) (fun x -> (x,x))) in  
  let u1 = M.set u0 39 120 in 
  b __LOC__
  (A.every2 (M.toArray u0) 
   (A.map (I.range 0 39) (fun x -> (x,x)))
   (fun (x0,x1) (y0,y1) -> x0 = y0 && x1 = y1));
  
  b __LOC__
  (L.every2 
    (M.toList u0)
    (L.ofArray (A.map (I.range 0 39) (fun  x -> (x,x))))
    (fun (x0,x1) (y0,y1) -> x0 = y0 && x1 = y1));
  eq __LOC__ (M.get u0 39) (Some 39);
  eq __LOC__ (M.get u1 39) (Some 120)


let () =     
  let u = f 
    ((A.makeByAndShuffle 10_000 (fun x  -> (x,x)))) in 
 eq __LOC__    
  (A.makeBy 10_000 (fun x  -> (x,x)))
  (M.toArray u)


;; Mt.from_pair_suites __FILE__ !suites  
