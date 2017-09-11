
let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites





(* module X =  Map.Make(String) *)




module Y0 = Functor_def.Make(Functor_inst)

module Y1 = Functor_def.Make(Functor_inst)


;; eq __LOC__  (Y0.h 1 2 ) 4
;; eq __LOC__ (Y1.h 2 3 ) 6

let v = Functor_def.return ()


;; eq __LOC__ v 2 

;; Mt.from_pair_suites __FILE__ !suites