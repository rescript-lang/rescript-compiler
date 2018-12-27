
(* TODO: is it good or or bad to change arity of [f], 
   actually we can not, since we can not tell from the lambda layer
*)
let f  h () = 
  fun x y -> h x y


let f h () = 
  let u = 1 + 2 in 
  Js.log u ; 
  fun  x y -> h  x y

;; Mt.from_pair_suites __MODULE__ Mt.[
    __LOC__, (fun _ -> Eq(f (+) () 1 2, 3 ))
]
