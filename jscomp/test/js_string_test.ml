
let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0

let eq loc ((x, y) as _z) =
  Js.log _z;
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites


let () =
  eq __LOC__ ("012", "0" |> Js.String.concat  [|"1";"2"|])

let () =
  Mt.from_pair_suites __FILE__ !suites
