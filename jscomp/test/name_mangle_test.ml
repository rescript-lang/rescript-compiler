let suites : Mt.pair_suites ref = ref []
let test_id = ref 0

let eq loc x y =
  incr test_id ;
  suites :=
    (loc ^ " id " ^ string_of_int !test_id, fun _ -> Mt.Eq (x, y)) :: !suites

(* FIXME: paren needed here {[ x##xh#= (g z ) ]} *)
let f0 (x : < _open: int [@bs.set] > Js.t) =
  let old = x##_open in
  x ## _open #= (old + 1) ;
  x##_open

let f1 (x : < _in: int [@bs.set] > Js.t) =
  let old = x##_in in
  x ## _in #= (old + 1) ;
  x##_in

let f2 (x : < _MAX_LENGTH: int [@bs.set] > Js.t) =
  let old = x##_MAX_LENGTH in
  x ## _MAX_LENGTH #= (old + 1) ;
  x##_MAX_LENGTH

let f3 (x : < _Capital: int [@bs.set] > Js.t) =
  let old = x##_Capital in
  x ## _Capital #= (old + 1) ;
  x##_Capital

let f4 (x : < _open__: int [@bs.set] > Js.t) =
  let old = x##_open__ in
  x ## _open__ #= (old + 1) ;
  x##_open__

let f5 (x : < open__: int [@bs.set] > Js.t) =
  let old = x##open__ in
  x ## open__ #= (old + 1) ;
  x##open__

(* < _ : int > -> is a syntax error *)

let f6 (x : < _'x: int [@bs.set] > Js.t) =
  let old = x##_'x in
  x ## _'x #= (old + 1) ;
  x##_'x

let f7 (x : < _Capital__: int [@bs.set] > Js.t) =
  let old = x##_Capital__ in
  x ## _Capital__ #= (old + 1) ;
  x##_Capital__

let f8 (x : < _MAX__: int [@bs.set] > Js.t) =
  let old = x##_MAX__ in
  x ## _MAX__ #= (old + 1) ;
  x##_MAX__

let f9 (x : < __: int [@bs.set] > Js.t) =
  let old = x##__ in
  x ## __ #= (old + 1) ;
  x##__

let f10 (x : < __x: int [@bs.set] > Js.t) =
  let old = x##__x in
  x ## __x #= (old + 1) ;
  x##__x

(* triple _ *)
let f11 (x : < ___: int [@bs.set] > Js.t) =
  let old = x##___ in
  x ## ___ #= (old + 1) ;
  x##___

(* quad _ *)
let f12 (x : < ____: int [@bs.set] > Js.t) =
  let old = x##____ in
  x ## ____ #= (old + 1) ;
  x##____

let () =
  eq __LOC__ (f0 [%raw "{open:0}"]) 1 ;
  eq __LOC__ (f1 [%raw "{in:0}"]) 1 ;
  eq __LOC__ (f2 [%raw "{MAX_LENGTH:0}"]) 1 ;
  eq __LOC__ (f3 [%raw "{Capital:0}"]) 1 ;
  eq __LOC__ (f4 [%raw "{_open:0}"]) 1 ;
  eq __LOC__ (f5 [%raw "{open:0}"]) 1 ;
  eq __LOC__ (f6 [%raw {|{ "'x" :0} |}]) 1 ;
  eq __LOC__ (f7 [%raw "{_Capital:0}"]) 1 ;
  eq __LOC__ (f8 [%raw "{_MAX:0}"]) 1 ;
  eq __LOC__ (f9 [%raw "{__:0}"]) 1 ;
  eq __LOC__ (f10 [%raw "{__x:0}"]) 1 ;
  eq __LOC__ (f11 [%raw "{_:0}"]) 1 ;
  eq __LOC__ (f12 [%raw "{__:0}"]) 1

;;
Mt.from_pair_suites __LOC__ !suites
