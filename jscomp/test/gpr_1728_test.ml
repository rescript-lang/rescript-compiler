let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites




let foo x = int_of_string x != 3

let badInlining obj =
  if foo obj##field then ()
  

;; eq __LOC__ (badInlining [%obj{field = "3" }]) ()

;; eq __LOC__ (int_of_string "-13") (-13)
;; eq __LOC__ (int_of_string "+13") 13
;; eq __LOC__ (int_of_string "13") 13
;; eq __LOC__ (int_of_string "0u32") 32
;; eq __LOC__ (int_of_string "-0u32") (-32)
;; eq __LOC__ (int_of_string "+0u32") 32
;; eq __LOC__ (int_of_string "+0x32") 50
;; eq __LOC__ (int_of_string "-0x32") (-50)
;; eq __LOC__ (int_of_string "0x32") 50
;; Mt.from_pair_suites __FILE__ !suites