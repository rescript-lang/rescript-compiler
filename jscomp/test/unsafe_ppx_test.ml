


let x : string = [%js.raw{|"\x01\x02\x03"|}]

let max : float -> float -> float = [%js.raw{|Math.max|}[@arity 2] (* TODO *) ]  

(* let max2 : float -> float -> float = [%js.raw {Math.max} ]   *)
[%%js.raw {|

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



