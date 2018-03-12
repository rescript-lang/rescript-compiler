
let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y 


let nearestGroots = []

let oppHeroes = [0]
let huntGrootCondition =
  List.length nearestGroots > 0 &&
     let x = (List.filter (fun h  -> (List.hd nearestGroots) <= 1000) 
        oppHeroes) in 
        List.length x = 0

let () = 
  eq __LOC__ huntGrootCondition false
let () =         
  Mt.from_pair_suites __FILE__ !suites