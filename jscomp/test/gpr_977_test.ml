let suites : Mt.pair_suites ref = ref []
let test_id = ref 0

let eq loc x y =
  incr test_id ;
  suites :=
    (loc ^ " id " ^ string_of_int !test_id, fun _ -> Mt.Eq (x, y)) :: !suites

let f x =
  for i = 0 to 100 do
    Js.log "." (* prevent optimization*)
  done ;
  -x

let int32_f x =
  for i = 0 to 100 do
    Js.log "." (* prevent optimization*)
  done ;
  Int32.neg x

let nint32_f x =
  for i = 0 to 100 do
    Js.log "." (* prevent optimization*)
  done ;
  Nativeint.neg x

let min_32_int = -2147483648
let u = f min_32_int

let () =
  eq __LOC__ min_32_int u ;
  eq __LOC__ Int32.min_int (int32_f Int32.min_int) ;
  eq __LOC__ (nint32_f (-2147483648n)) 2147483648n

let () = Mt.from_pair_suites __MODULE__ !suites
