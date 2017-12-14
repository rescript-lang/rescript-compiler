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

module N = Bs.SetInt


let (=~) s i = 
  N.(eq (ofArray i) s)
let (=*) a b =  
  N.(eq (ofArray a) (ofArray b))
let ofA = N.ofArray

let ()= 
  b __LOC__ 
  ([|1;2;3|] =*
    [|3;2;1|])
let u = N.(inter (ofA [|1;2;3|]) (ofA [|3;4;5|]) )

let ()= 
  b __LOC__ 
  (u
  =~ [|3|])

(* inclusive *)
let range i j = 
  Array.init (j - i + 1) (fun k -> k + i )

let () = 
  b __LOC__ (N.(inter (ofA (range 1 100)) (ofA (range 50 200))   ) =~ range 50 100)

let () = 
  b __LOC__ (N.(union (ofA (range 1 100)) (ofA (range 50 200))   ) =~ range 1 200)

let () =
    b __LOC__ (N.(diff (ofA (range 1 100)) (ofA (range 50 200))   ) =~ range 1 49)

;; Mt.from_pair_suites __FILE__ !suites    