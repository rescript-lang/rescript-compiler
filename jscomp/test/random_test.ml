let id = ref 0
let suites = ref []
let eq f = Mt_global.collect_eq id suites f
let neq f = Mt_global.collect_neq id suites f
let approx f = Mt_global.collect_approx id suites f

let () =
  neq __LOC__
    (Random.self_init () ; Random.int 10000)
    (Random.self_init () ; Random.int 1000)

(* determinism acutally *)
let v = Random.init 0
let v = Array.make 10 false

let () =
  for i = 0 to 9 do
    v.(i) <- Random.bool ()
  done

let () =
  eq __LOC__ v [|true; true; true; true; true; false; true; true; true; false|]

let f = Random.int64 Int64.max_int
let h = Random.int64 3L
let vv = Random.bits ()
let xx = Random.float 3.0
let xxx = Random.int32 103l
let () = Printf.printf "%Ld %Ld %d %f %ld \n" f h vv xx xxx
let () = Mt.from_pair_suites __MODULE__ !suites
