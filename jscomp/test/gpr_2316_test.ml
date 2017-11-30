let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites


let y = 
  match failwith "boo" with 
  | exception Failure msg -> Some msg 
  | e -> None 

let x =
  match failwith "boo" with
  | exception Failure msg -> Some msg
  | e -> (Js.log "ok"; None)

let () =   
  eq __LOC__ y (Some "boo");
  eq __LOC__ x (Some "boo")


;; Mt.from_pair_suites __FILE__ !suites  