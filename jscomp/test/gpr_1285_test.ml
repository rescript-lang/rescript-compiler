let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites


let step1 () =
  object
    method step2 =
      object
        method step3 = 33
      end
  end

let x = ((step1 ()) # step2 ) # step3

let () = 
  eq __LOC__ x 33 


;; Mt.from_pair_suites __FILE__ !suites  

