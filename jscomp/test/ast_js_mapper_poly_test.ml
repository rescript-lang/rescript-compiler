
let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites


type u = 
  [ `D 
  | `C 
  | `f [@bs.as "x"]
  ]
[@@bs.deriving jsMapper]

let eqU (x : u) (y : u) = x = y
let eqUOpt (x : u option) y = 
  match x,y with 
  | Some x, Some y -> x = y 
  | None, None -> true
  | _, _ -> false 

let () =   
  eq __LOC__ (eqUOpt (uFromJs "x") (Some `f )) true;
  eq __LOC__ (eqUOpt (uFromJs "D") (Some `D )) true;
  eq __LOC__ (eqUOpt (uFromJs "C") (Some `C )) true;
  eq __LOC__ (eqUOpt (uFromJs "f") (None )) true;
  eq __LOC__ (Array.map uToJs [|`D; `C ; `f|]) [|"D"; "C"; "x"|]



type v = 
  | A0 
  | A1 [@bs.as 3]
  | A2
  | A3 
[@@bs.deriving jsMapper]

let eqV (x : v) (y : v) = x = y
let eqVOpt (x : v option) y= 
  match x,y with 
  | Some x, Some y -> x = y 
  | None, None -> true
  | _, _ -> false 

let s = function 
  | A0 -> "A0"
  | A1 -> "A1"
  | A2 -> "A2"
  | A3 -> "A3"

let () = 
  eq __LOC__ (Array.map vToJs [|A0;A1;A2;A3|]) [|0;3;4;5|];
  eq __LOC__ (Array.map vFromJs [|0;1;2;3;4;5;6|])
  [|Some A0; None; None; Some A1; Some A2; Some A3; None|]


type v1 =     
  | B0 
  | B1 
  | B2 
  | B3 
  | B4 
  | B5 
[@@bs.deriving jsMapper]
let () = 
  eq __LOC__ (Array.map v1ToJs [|B0;B1;B2;B3;B4;B5|]) [|0;1;2;3;4;5|];
  eq __LOC__ (Array.map v1FromJs [|-1;0;1;2;3;4;5;6|])
  [|None;Some B0; Some B1; Some B2; Some B3; Some B4; Some B5; None|]

(** TODO: add jsType support *)  
type v2 =  
  | C0  [@bs.as 2 ]
  | C1
  | C2 
  | C3 
  | C4
  | C5 
[@@bs.deriving jsMapper ]


;;  
let (+>) = Array.append
let () = 
  eq __LOC__ 
  (Array.map v2ToJs [|C0; C1; C2 ; C3 ; C4; C5 |])
  [|2;3;4;5;6;7|];
  eq __LOC__
  (Array.map v2FromJs [|0;1;2;3;4;5;6;7;8|])
  (
    [|None;None|]+>
    (Array.map (fun x -> Some x) [|C0; C1; C2 ; C3 ; C4; C5 |]) +>
    [|None|]
  )

;; Mt.from_pair_suites  __FILE__ !suites