module type X = module type of String


let u (v : (module X)) = v

module N = 
struct 
  let s =  u (module String)
end

let v0 = 
  let module V = (val N.s : X ) in V.length "x" 

let v x = 
  let module V = (val N.s : X ) in V.length x


let suites = Mt.[
    "const", (fun _ -> Eq(1,v0));
    "other", (fun _ -> Eq(3,v "abc"))     
]

;; Mt.from_pair_suites __MODULE__ suites
