


let x : string = [%bs.raw{|"\x01\x02\x03"|}]

let max  = [%bs.raw ( "Math.max"  : float -> float -> float)  ]  

let u = max 1. 
(* let max2 : float -> float -> float = [%bs.raw {Math.max} ]   *)
[%%bs.raw {|

function $$test(x,y){
  return x + y;
}
|}]

let max2 : float -> float -> float = [%bs.raw "Math.max"]
let u = max2 3.

let max3 = ([%bs.raw "Math.max"] :  float -> float -> float)
let uu = max2 3.
    
external test : int -> int -> int = "" [@@bs.call "$$test"]

let empty = ([%bs.raw ({| Object.keys|}  : _ -> string array) ]) 3 

let v = test 1 2 

;; Mt.from_pair_suites __FILE__ Mt.[
    "unsafe_max", (fun _ -> Eq(2., max 1. 2.));
    "unsafe_test", (fun _ -> Eq(3,v));
    "unsafe_max2", (fun _ -> Eq(2, [%bs.raw({|Math.max|} : int -> int -> int)] 1 2 ));
    "ffi_keys", ( fun _ -> Eq ([|"a"|], Ffi_js.keys [%bs.raw{| {a : 3}|}]))
]




