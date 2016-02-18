

let u = ref 3 
let v = lazy ( u := 32)

let lazy_test () = 
  let h = !u in
  let g = (Lazy.force v ; !u) in
  h,g


(** this is broken due to 
    [%obj_field] and [%obj_set_field] is translated into 
    [%array_unsafe_get] which does not fit our purpose
    http://caml.inria.fr/mantis/view.php?id=7020
*)
;; Mt.from_pair_suites __FILE__ Mt.[
"simple", (fun _ ->
  Eq ((lazy_test ()) ,(3, 32))
          )
]

