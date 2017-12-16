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

module N = Bs.MapInt

let () = 
  let v = Bs.Array.init 1_000_000 (fun[@bs] i -> (i,i)) in 
  Bs.Array.shuffleInPlace v ; 
  let u = N.ofArray v in   
  b __LOC__ (N.checkInvariant u);
  let firstHalf = Bs.Array.sub v 0 2_000 in 
  let xx = Bs.Array.foldLeft 
      (fun[@bs] acc (x,_) -> N.remove x acc) u firstHalf in 
  b __LOC__ (N.checkInvariant u);


;; Mt.from_pair_suites __FILE__ !suites