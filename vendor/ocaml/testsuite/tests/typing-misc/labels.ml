(* PR#5835 *)
let f ~x = x + 1;;
f ?x:0;;

(* PR#6352 *)
let foo (f : unit -> unit) = ();;
let g ?x () = ();;
foo ((); g);;

(* PR#5748 *)
foo (fun ?opt () -> ()) ;; (* fails *)
