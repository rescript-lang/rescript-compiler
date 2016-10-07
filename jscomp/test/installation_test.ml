let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites




let _ : _ Js.undefined = 
  Js.Undefined.bind [%node __dirname] (fun [@bs] p -> 
      let bsc_exe = 
        Node.Path.join 
          [| p ;  ".."; "bin"; "bsc.exe" |] in 
      let output = 
        Node.Child_process.execSync 
          (bsc_exe ^ " -where ") 
          (Node.Child_process.option  ~encoding:"utf8" ()) in 
      let dir = Js.String.trim output in 
      let exists = 
        (Node.Fs.readdirSync dir ) 
        |> Js.Array.includes "pervasives.cmi" in 
      eq __LOC__ exists Js.true_

    )

let () = 
  Mt.from_pair_suites __FILE__ !suites
