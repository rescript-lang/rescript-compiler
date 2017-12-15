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

let revRange i j = 
    Array.init (j - i + 1) (fun k -> k + i ) |> Array.to_list |> List.rev |> Array.of_list     

let () = 
  let v = (ofA (Array.append (range 100 1000) (revRange  400 1500))) in     
  b __LOC__ (v =~ (range 100 1500));
  let l, r = N.partition (fun[@bs] x -> x mod 3 = 0) v in 
  let nl, nr = 
    let l,r = ref N.empty, ref N.empty in 
    for i = 100 to 1500 do 
      if i mod 3 = 0 then 
        l := N.add i !l 
      else 
        r := N.add i !r
    done; 
    !l, !r in 
  b __LOC__ (N.eq l nl);
  b __LOC__ (N.eq r nr) 
  
let () = 
  b __LOC__ (N.(inter (ofA (range 1 100)) (ofA (range 50 200))   ) =~ range 50 100)

let () = 
  b __LOC__ (N.(union (ofA (range 1 100)) (ofA (range 50 200))   ) =~ range 1 200)

let () =
    b __LOC__ (N.(diff (ofA (range 1 100)) (ofA (range 50 200))   ) =~ range 1 49)

let () = 
  b __LOC__ (N.(inter (ofA (revRange 1 100)) (ofA (revRange 50 200))   ) =~ revRange 50 100)

let () = 
  b __LOC__ (N.(union (ofA (revRange 1 100)) (ofA (revRange 50 200))   ) =~ revRange 1 200)

let () =
    b __LOC__ (N.(diff (ofA (revRange 1 100)) (ofA (revRange 50 200))   ) =~ revRange 1 49)
    
let () =     
  let ss = [|1;222;3;4;2;0;33;-1|] in 
  let v = ofA [|1;222;3;4;2;0;33;-1|] in 
  let minv, maxv = N.min v, N.max v in 
  eq __LOC__ (N.fold (fun [@bs] x y -> x + y) v 0) (Array.fold_left (+) 0 ss) ;
  eq __LOC__ minv (Some (-1)); 
  eq __LOC__ maxv (Some 222);
  let v = N.remove 3 v in 
  let minv, maxv = N.min v, N.max v in 
  eq __LOC__ minv (Some (-1)); 
  eq __LOC__ maxv (Some 222);
  let v = N.remove 222 v in 
  let minv, maxv = N.min v, N.max v in 
  eq __LOC__ minv (Some (-1)); 
  eq __LOC__ maxv (Some 33);
  let v = N.remove (-1) v in 
  let minv, maxv = N.min v, N.max v in 
  eq __LOC__ minv (Some (0)); 
  eq __LOC__ maxv (Some 33);
  let v = N.remove 0  v in 
  let v = N.remove 33 v in  
  let v = N.remove 2  v in 
  let v = N.remove 3 v in  
  let v = N.remove 4 v in 
  let v = N.remove 1 v in 
  b __LOC__ (N.isEmpty v )



;; Mt.from_pair_suites __FILE__ !suites    