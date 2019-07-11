(* (Printf.sprintf *)
(* (/* Format */[0, *)
(* /* Int */[4, *)
(* /* Int_d */0, *)
(* /* No_padding */0, *)
(* /* No_precision */0, *)
(* /* String */[2,/* No_padding */0,/* End_of_format */0]], *)
(* "%d%s"]) *)
(* (32,"ss")) *)

(* working version *)
(* (Printf.sprintf *)
(* (/* Format */[0, *)
(* /* Int */[4, *)
(* /* Int_d */0, *)
(* /* No_padding */0, *)
(* /* No_precision */0, *)
(* /* String */[2,/* No_padding */0,/* End_of_format */0]], *)
(*       "%d%s"]) *)
(*    (32)("ss")) *)

;;
prerr_endline @@ Printf.sprintf "%d%s" 32 "ss"
