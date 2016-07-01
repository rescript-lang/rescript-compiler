


let x : string = [%bs.raw{|"\x01\x02\x03"|}]

let max : float -> float -> float [@fn] =
  [%bs.raw "Math.max"  ]  

let u v = max 1. v [@fn] 
(* let max2 : float -> float -> float = [%bs.raw {Math.max} ]   *)
[%%bs.raw {|

function $$test(x,y){
  return x + y;
}
|}]


let regression3 : float -> float -> float [@fn] = [%bs.raw "Math.max"] 

let regression4 : float ->  (float -> float [@fn]) -> float [@fn] =
  [%bs.raw "Math.max"] 
let g a 

  = 
let regression  = ([%bs.raw{|function(x,y){
   return ""
}|}]  : float -> (string -> 'a) -> string) in 

  let regression2 : float -> float -> float = [%bs.raw "Math.max"] in 
  ignore @@ regression a failwith;
  ignore @@ regression2  3. 2.;
  ignore @@ regression3 3.  2. [@fn];
  ignore @@ regression4 3. (fun[@fn] x-> x) [@fn]


let max2 : float -> float -> float [@fn] = [%bs.raw "Math.max"]

let umax a b = max2 a b  [@fn]
let u h = max2 3. h [@fn]

let max3 = ([%bs.raw "Math.max"] :  float * float -> float [@fn])
let uu h = max2 3. h [@fn]
    
external test : int -> int -> int = "" [@@bs.call "$$test"]

let empty = ([%bs.raw {| Object.keys|} ] :  _ -> string array [@fn]) 3 [@fn]

let v = test 1 2 



;; Mt.from_pair_suites __FILE__ Mt.[
    "unsafe_max", (fun _ -> Eq(2., max 1. 2. [@fn]));
    "unsafe_test", (fun _ -> Eq(3,v));
    "unsafe_max2", (fun _ -> Eq(2, ([%bs.raw {|Math.max|} ] : int ->  int -> int [@fn]) 1 2 [@fn] ));
    "ffi_keys", ( fun _ -> Eq ([|"a"|], Ffi_js.keys [%bs.raw{| {a : 3}|}] [@fn]))
]




