
let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites

let check_healty check =
  not (check##a )&& not (check##b) && not (check##c)


let basic_not x = 
  not x 

let f check = 
  check##x && check##y 
  (* [x && y] in OCaml can be translated to [x && y] in JS  *)


let ()
  = 
  eq __LOC__ (f [%obj{x=true;y=false}]) false

let () = 
  eq __LOC__ (check_healty [%obj{a=false;b=false;c=true}]) false



let () = Mt.from_pair_suites __MODULE__ !suites
