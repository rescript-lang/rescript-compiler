


let x : string = [%js.raw{|"\x01\x02\x03"|}]

let max  = [%js.raw ( "Math.max"  : float -> float -> float)  ]  

let u = max 1. 
(* let max2 : float -> float -> float = [%js.raw {Math.max} ]   *)
[%%js.raw {|

function $$test(x,y){
  return x + y;
}
|}]

external test : int -> int -> int = "" [@@js.call "$$test"]

let empty = ([%js.raw ({| Object.keys|}  : _ -> string array) ]) 3 

let v = test 1 2 

;; Mt.from_pair_suites __FILE__ Mt.[
    "unsafe_max", (fun _ -> Eq(2., max 1. 2.));
    "unsafe_test", (fun _ -> Eq(3,v));
    "unsafe_max2", (fun _ -> Eq(2, [%js.raw({|Math.max|} : int -> int -> int)] 1 2 ));
    "ffi_keys", ( fun _ -> Eq ([|"a"|], Ffi_js.keys [%js.raw{| {a : 3}|}]))
]




