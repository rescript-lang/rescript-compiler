let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc (x, y) = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites



external joinClasses : int array  -> int = "./joinClasses"
  [@@bs.module] [@@bs.variadic]


let a =
  joinClasses [|1;2;3|]


let () = 
  let pair = (a,6) in
  Js.log pair ;
  eq __LOC__ pair

let () =
  Mt.from_pair_suites  __MODULE__ !suites
