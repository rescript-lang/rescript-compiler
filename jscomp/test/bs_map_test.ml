let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites

let b loc v  = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), 
     (fun _ -> Mt.Ok v)) :: !suites

module M = Belt.Map.Int
module N = Belt.Set.Int
module A = Belt.Array


let mapOfArray x = M.fromArray x 
let setOfArray x = N.fromArray  x 
let emptyMap () = M.empty 

let () = 
  let v = 
      (A.makeByAndShuffle 1_000_000 (fun i -> (i,i))) in 
  let u = M.fromArray v in   
   (M.checkInvariantInternal u);
  let firstHalf = A.slice v 0 2_000 in 
  let xx = A.reduce firstHalf u
      (fun acc (x,_) -> M.remove acc x)  in 
  (M.checkInvariantInternal u);
  (M.checkInvariantInternal xx);


;; Mt.from_pair_suites __FILE__ !suites
