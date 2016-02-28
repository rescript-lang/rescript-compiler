


let x : string = [%bb.unsafe{|"\x01\x02\x03"|}]

let max : float -> float -> float = [%bb.unsafe{|Math.max|}[@arity 2] (* TODO *) ]  

(* let max2 : float -> float -> float = [%bb.unsafe {Math.max} ]   *)

;; Mt.from_pair_suites __FILE__ Mt.[
    "unsafe_max", (fun _ -> Eq(2., max 1. 2.))
]



