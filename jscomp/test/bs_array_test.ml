
let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites
let neq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Neq(x,y))) :: !suites


type 'a t = 'a Js.Array.t
let () =
  [| 1; 2; 3; 4 |]
  |> Js.Array.filter (fun  x -> x > 2)
  |> Js.Array.mapi (fun  x i -> x + i)
  |> Js.Array.reduce (fun  x y -> x + y) 0 
  |> Js.log



let id x = 
  eq __LOC__ 
   (Js.Vector.toList @@ Js.List.toVector x ) x 

let () =
  eq __LOC__ (Js.List.toVector [1;2;3]) [|1;2;3|];
  eq  __LOC__ 
  ( Js.Vector.map (fun [@bs] x -> x + 1) [|1;2;3|] )
  [|2;3;4|];
  eq __LOC__  (Js.Vector.make 5 3)
    [|3;3;3;3;3|];
  eq __LOC__ 
  ( let a = Js.Vector.init 5  (fun [@bs] i -> i + 1) in 
    Js.Vector.filterInPlace (fun [@bs] j -> j mod 2 = 0) a ; 
    a 
  )
  [|2;4|];

  eq __LOC__ 
  ( let a = Js.Vector.init 5  (fun [@bs] i -> i + 1) in 
    Js.Vector.filterInPlace (fun [@bs] j -> j mod 2 <> 0) a ; 
    a 
  )
  [|1;3;5|];

  eq __LOC__  
    (Js.List.toVector [1;2;3] ) [|1;2;3|];
  eq __LOC__
    (Js.List.toVector [1])   [|1|];
  id []  ;
  id [1];
  id [1;2;3;4;5];
  id (Js.Vector.(toList @@ init 100 (fun [@bs] i -> i  ) ))
module A = Bs.Array  
let add = fun [@bs] x y -> x + y
let () = 
  let v = Bs.Array.init 3000 (fun[@bs] i -> i) in 
  let u = Bs.Array.copy v  in 
  Bs.Array.shuffleDone u ; 
  neq __LOC__ u  v (* unlikely*);
  let sum x = Bs.Array.foldLeft x 0 add in 
  eq __LOC__ ( sum u) (sum v)
let addone = fun [@bs] x -> x + 1

let () =   
  eq __LOC__ (A.init 0 begin fun[@bs] _ ->1 end ) [||];
  eq __LOC__ (A.init 3 begin fun [@bs] i -> i end) [|0;1;2|];
  eq __LOC__ (A.makeMatrix 3 4 1 
    
  ) [| [|1;1;1;1|]; [|1;1;1;1|]; [|1;1;1;1|]|];
  eq __LOC__ (A.makeMatrix 3 0 0 ) [| [||] ; [||]; [||] |];
  eq __LOC__ (A.makeMatrix  0 3 1 ) [||];
  eq __LOC__ (A.makeMatrix 1 1 1) [| [|1 |] |];
  eq __LOC__ (A.copy [||]) [||];
  eq __LOC__ (A.map [||] addone) [||];
  eq __LOC__ (A.mapi [||] add) [||];
  eq __LOC__ (A.mapi [|1;2;3|] add) [|1;3;5|];
  eq __LOC__ (A.toList [||]) [];
  eq __LOC__ (A.toList [|1|]) [1];
  eq __LOC__ (A.toList [|1;2;3|]) [1;2;3];
  eq __LOC__ (A.map [|1;2;3|] addone) [|2;3;4|];
  eq __LOC__ (A.ofList []) [||];
  eq __LOC__ (A.ofList [1]) [|1|];
  eq __LOC__ (A.ofList [1;2]) [|1;2|];
  eq __LOC__ (A.ofList [1;2;3]) [|1;2;3|];


;; Mt.from_pair_suites __LOC__ !suites  