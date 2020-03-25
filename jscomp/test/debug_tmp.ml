
[@@@bs.config {
  flags = [|
  "-drawlambda"; "-dlambda"; 
  "-dtypedtree"
  (* "-bs-debug-unit" *)
  |]
}]

(* let u = Some (Some (Some ())) *)
let u = fun () -> 2
(* let u o = o##hi 1 2 *)

(*
(setglobal Debug_tmp!
  (let
    (u/1002 =
       (function o/1003
         (#full_apply
           (apply (opaque (sendhi (#unsafe_downgrade o/1003) 23297)) 1 2))))
    (makeblock 0 u/1002)))

opaque prevents fusing of Lsend    
*)