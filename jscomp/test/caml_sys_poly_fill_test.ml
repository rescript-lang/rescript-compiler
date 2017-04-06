let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites
 

let () =

  eq __LOC__ "X" (
    Node_process.putEnvVar __FILE__ "X";
    let v = Sys.getenv __FILE__ in 
    Node_process.deleteEnvVar __FILE__ ;
    v
  );
  eq __LOC__ "Y" (
    Node_process.putEnvVar __FILE__ "Y";
    let v = Sys.getenv __FILE__ in 
    Node_process.deleteEnvVar __FILE__ ;
    v
  );
  eq __LOC__ "Z" (
    Node_process.deleteEnvVar __FILE__ ;
    let v = try Sys.getenv __FILE__ with Not_found -> "Z" in 
    v
  )



let () = Mt.from_pair_suites __FILE__ !suites
