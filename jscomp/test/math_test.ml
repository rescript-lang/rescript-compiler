let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites



let () =
  begin 
    eq __LOC__ (Js.Math.ceil 3.2) 4;
    eq __LOC__ (Js.Math.floor 3.2) 3;
    eq __LOC__ true
      (let a = Js.Math.random_int 1 3 in a >= 1 && a < 3 )
  end
 
let () = Mt.from_pair_suites __FILE__ !suites
