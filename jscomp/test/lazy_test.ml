let u = ref 3
let v = lazy (u := 32)

let lazy_test () =
  let h = !u in
  let g = Lazy.force v ; !u in
  (h, g)

let f = function
  | (lazy ()), _, {contents= None} -> 0
  | _, (lazy ()), {contents= Some x} -> 1

(* PR #5992 *)
(* Was segfaulting *)
let s = ref None
let set_true = lazy (s := Some 1)
let set_false = lazy (s := None)
let h = try f (set_true, set_false, s) with Match_failure _ -> 2
let u_v = ref 0
let u = lazy (u_v := 2)
let () = Lazy.force u

(** this is broken due to [%obj_field] and [%obj_set_field] is translated into
    [%array_unsafe_get] which does not fit our purpose
    http://caml.inria.fr/mantis/view.php?id=7020 *)

(* module Mt = Mock_mt *)

let exotic = (* Lazy in a pattern. (used in advi) *)
  function (lazy y) -> y

(* let l_from_val = Lazy.from_val 3 *)

let l_from_fun = Lazy.from_fun (fun _ -> 3)

let forward_test =
  lazy
    (let u = ref 3 in
     incr u ; !u)

(* module Mt = Mock_mt *)

;;
Mt.from_pair_suites __MODULE__
  Mt.
    [ ("simple", fun _ -> Eq (lazy_test (), (3, 32)))
    ; ("lazy_match", fun _ -> Eq (h, 2))
    ; ("lazy_force", fun _ -> Eq (!u_v, 2))
    ; ("lazy_from_fun", fun _ -> Eq (Lazy.force l_from_fun, 3))
    ; ("lazy_from_val", fun _ -> Eq (Lazy.force (Lazy.from_val 3), 3))
    ; ( "lazy_from_val2"
      , fun _ -> Eq (Lazy.force @@ Lazy.force (Lazy.from_val (lazy 3)), 3) )
    ; ( "lazy_from_val3"
      , fun _ ->
          Eq
            ( ( [%bs.debugger] ;
                Lazy.force @@ Lazy.force (Lazy.from_val forward_test) )
            , 4 ) ) ]
