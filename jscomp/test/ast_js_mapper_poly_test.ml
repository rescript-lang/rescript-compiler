
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
[@@bs.deriving jsConverter]

let eqU (x : u) (y : u) = x = y
let eqUOpt (x : u option) y = 
  match x,y with 
  | Some x, Some y -> x = y 
  | None, None -> true
  | _, _ -> false 

type v = 
  | A0 
  | A1 [@bs.as 3]
  | A2
  | A3 
[@@bs.deriving jsConverter]

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
  eq __LOC__ (Array.map vToJs [|A0;A1;A2;A3|]) [|0;3;4;5|]


(* ;; Mt.from_pair_suites  __MODULE__ !suites *)
(* XXX TODO: [@bs.as 3] does not make sense in this representation *)