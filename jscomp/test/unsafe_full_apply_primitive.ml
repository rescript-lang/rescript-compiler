

(* external unsafeInvariantApply : 'a -> 'a = "#full_apply"


let f1 x = unsafeInvariantApply (x ())


let f2 x y = unsafeInvariantApply (x y ()) *)


let rec f = fun [@bs] a -> f a [@bs]


(* not allowed due to special encoding of unit *)
(* let rec f1 = fun [@bs] () -> f1 () [@bs] *)
(* let rec f2 = Sys.opaque_identity (fun () -> f2 ()) *)

