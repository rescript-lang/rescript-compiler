

open Stack 

let to_list v =
  let acc = ref [] in
  while not @@ is_empty v do
    acc := pop v :: !acc 
  done;
  List.rev !acc 
    
let v () = 
  let v = create ()in
  push 3 v; 
  push 4 v;
  push 1 v ;
  to_list v 

let suites = 
  ["push_test", (fun _ -> Mt.Eq ([1;4;3], v ()))]


;; Mt.from_pair_suites __MODULE__ suites
  
