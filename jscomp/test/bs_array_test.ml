
let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites


type 'a t = 'a Js.Array.t
let () =
  [| 1; 2; 3; 4 |]
  |> Js.Array.filter (fun  x -> x > 2)
  |> Js.Array.mapi (fun  x i -> x + i)
  |> Js.Array.reduce (fun  x y -> x + y) 0 
  |> Js.log




let () =
  eq __LOC__ (Bs.Array.ofList [1;2;3]) [|1;2;3|];
  eq  __LOC__ 
  ( Bs.Array.map (fun [@bs] x -> x + 1) [|1;2;3|] )
  [|2;3;4|]

;; Mt.from_pair_suites __LOC__ !suites  