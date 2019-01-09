let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y =
  incr test_id ;
  suites :=
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites


let () =
  eq __LOC__
    (Js.Array2.reduce [|1;2;3;4;5;6;7;8;9;10;11;12;13;14|] (+) 0 )
    (Global_mangles.(
        __dirname +
        __filename +
        clearImmediate +
        clearInterval +
        clearTimeout +
        console +
        exports +
        global +
        _module +
        process +
        require +
        setImmediate +
        setInterval +
        setTimeout
      ))

let () = Mt.from_pair_suites __MODULE__ !suites
