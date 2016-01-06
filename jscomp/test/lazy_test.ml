

let u = ref 3 
let v = lazy ( u := 32)

let lazy_test (a,b)= 
  let h = !u in
  let g = (Lazy.force v ; !u) in
  h,g

(* open Mt  *)
(** this is broken due to 
    [%obj_field] and [%obj_set_field] is translated into 
    [%array_unsafe_get] which does not fit our purpose
    http://caml.inria.fr/mantis/view.php?id=7020
*)
(* ;; from_suites "lazy" [ *)
(* "simple", (fun _ ->  *)
(*   assert_equal (a,b) (3, 32) *)
(*           ) *)
(* ] *)

