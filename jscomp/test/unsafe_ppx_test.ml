


let x : string = [%bs.raw{|"\x01\x02\x03"|}]

let max : float * float -> float [@uncurry] =
  [%bs.raw "Math.max"  ]  

let u v = max (1.,v) [@uncurry] 
(* let max2 : float -> float -> float = [%bs.raw {Math.max} ]   *)
[%%bs.raw {|

function $$test(x,y){
  return x + y;
}
|}]


let regression3 : float * float -> float [@uncurry] = [%bs.raw "Math.max"] 

let regression4 : float * (float -> float [@uncurry]) -> float [@uncurry] =
  [%bs.raw "Math.max"] 
let g a 

  = 
let regression  = ([%bs.raw{|function(x,y){
   return ""
}|}]  : float -> (string -> 'a) -> string) in 

  let regression2 : float -> float -> float = [%bs.raw "Math.max"] in 
  ignore @@ regression a failwith;
  ignore @@ regression2  3. 2.;
  ignore @@ regression3 (3., 2.) [@uncurry];
  ignore @@ regression4 (3., (fun[@uncurry] x-> x)) [@uncurry]


let max2 : float * float -> float [@uncurry] = [%bs.raw "Math.max"]

let umax a b = max2 (a, b ) [@uncurry]
let u h = max2 (3., h) [@uncurry]

let max3 = ([%bs.raw "Math.max"] :  float * float -> float [@uncurry])
let uu h = max2 (3.,h) [@uncurry]
    
external test : int -> int -> int = "" [@@bs.call "$$test"]

let empty = ([%bs.raw {| Object.keys|} ] :  _ -> string array [@uncurry]) 3 [@uncurry]

let v = test 1 2 



;; Mt.from_pair_suites __FILE__ Mt.[
    "unsafe_max", (fun _ -> Eq(2., max (1., 2.) [@uncurry]));
    "unsafe_test", (fun _ -> Eq(3,v));
    "unsafe_max2", (fun _ -> Eq(2, ([%bs.raw {|Math.max|} ] : int * int -> int [@uncurry]) (1, 2)[@uncurry] ));
    "ffi_keys", ( fun _ -> Eq ([|"a"|], Ffi_js.keys [%bs.raw{| {a : 3}|}] [@uncurry]))
]




