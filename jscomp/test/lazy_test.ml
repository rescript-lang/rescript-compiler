

let u = ref 3 
let v = lazy ( u := 32)

let lazy_test () = 
  let h = !u in
  let g = (Lazy.force v ; !u) in
  h,g

let f = function
 | lazy (), _, {contents=None} -> 0
 | _, lazy (), {contents=Some x} -> 1

(* PR #5992 *)
(* Was segfaulting *)
let s = ref None
let set_true = lazy (s := Some 1)
let set_false = lazy (s := None)

let h =
   try f (set_true, set_false, s) with Match_failure _ -> 2 


(** this is broken due to 
    [%obj_field] and [%obj_set_field] is translated into 
    [%array_unsafe_get] which does not fit our purpose
    http://caml.inria.fr/mantis/view.php?id=7020
*)
(* module Mt = Mock_mt *)

let exotic = (* Lazy in a pattern. (used in advi) *)
function lazy y -> y

;; Mt.from_pair_suites __FILE__ Mt.[
"simple", (fun _ ->
  Eq ((lazy_test ()) ,(3, 32))
          );
"lazy_match", (fun _ -> Eq(h, 2))
]

