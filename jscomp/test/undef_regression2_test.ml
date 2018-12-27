
let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites


let ok loc x = 
  incr test_id; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Ok x)) :: !suites


let a = 
  match [%external ___undefined_value ] with   
  | None -> 1 
  | Some _ -> 2 


let test () = 
  match [%external __DEV__] with 
  | Some _ -> Js.log "dev mode"
  | None -> Js.log "producton mode"

let test2 () = 
  match [%external __filename] with 
  | Some f -> Js.log f 
  | None -> Js.log "non node environment"


let test3 () = 
  if [%external __DEV__] = None then
    Js.log "production mode"


let f x = 
  x = Js.undefined  



let () = 
  ok __LOC__ (a > 0);
  eq __LOC__ a 1


let () = Mt.from_pair_suites __MODULE__ !suites    