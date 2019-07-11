let suites : Mt.pair_suites ref = ref []
let test_id = ref 0

let eq loc x y =
  incr test_id ;
  suites :=
    (loc ^ " id " ^ string_of_int !test_id, fun _ -> Mt.Eq (x, y)) :: !suites

let a0 =
  try
    let _c = 0 / 0 in
    0
  with _ -> 1

let a1 =
  try
    let _h = 0 mod 0 in
    0
  with _ -> 1

(* let a2 = try (let _h = Nativeint.div 0n 0n in 0) with _ -> 1

   let a3 = try (let _h = Nativeint.rem 0n 0n in 0 ) with _ -> 1 *)
(* TODO: more work on int32 effect anaylsys*)

let a4 =
  try
    let _ = Int32.div 0l 0l in
    0
  with _ -> 1

let a5 =
  try
    let _ = Int32.rem 0l 0l in
    0
  with _ -> 1

let a6 =
  try
    let _ = Int64.div 0L 0L in
    0
  with _ -> 1

let a7 =
  try
    let _ = Int64.rem 0L 0L in
    0
  with _ -> 1

;;
eq __LOC__ (a0, a1, a4, a5, a6, a7) (1, 1, 1, 1, 1, 1)
;;
Mt.from_pair_suites __MODULE__ !suites
