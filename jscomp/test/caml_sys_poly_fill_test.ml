let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites
 

let () =

  eq __LOC__ "X" (
    Node_process.putEnvVar __MODULE__ "X";
    let v = Sys.getenv __MODULE__ in 
    Node_process.deleteEnvVar __MODULE__ ;
    v
  );
  eq __LOC__ "Y" (
    Node_process.putEnvVar __MODULE__ "Y";
    let v = Sys.getenv __MODULE__ in 
    Node_process.deleteEnvVar __MODULE__ ;
    v
  );
  eq __LOC__ "Z" (
    Node_process.deleteEnvVar __MODULE__ ;
    let v = try Sys.getenv __MODULE__ with Not_found -> "Z" in 
    v
  )

;; Js.log (Sys.getcwd(), Sys.time(),Sys.argv,Sys.executable_name)  


let () = Mt.from_pair_suites __MODULE__ !suites
