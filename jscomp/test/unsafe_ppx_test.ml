


let x : string = [%bb.unsafe{|"\x01\x02\x03"|}]

let max : float -> float -> float = [%bb.unsafe{|Math.max|}[@arity 2] (* TODO *) ]  

(* let max2 : float -> float -> float = [%bb.unsafe {Math.max} ]   *)
[%%bb.unsafe {|

function $$test(x,y){
  return x + y;
}
|}]

external test : int -> int -> int = "" [@@js.call "$$test"]

let v = test 1 2 

;; Mt.from_pair_suites __FILE__ Mt.[
    "unsafe_max", (fun _ -> Eq(2., max 1. 2.));
    "unsafe_test", (fun _ -> Eq(3,v))
]



