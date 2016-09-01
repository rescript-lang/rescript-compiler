let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites


let v = ref 0

external config : hi:int -> lo:int -> unit -> _ = "" [@@bs.obj]

let h =  config ~hi:2 ~lo:0 (ignore (incr v ))

let () = 
  eq __LOC__ !v 1 


let () = Mt.from_pair_suites __FILE__ !suites
