external of_int : int -> int64 = "%int64_of_int"
external of_nativeint : nativeint -> int64 = "%int64_of_nativeint"
external add : int64 -> int64 -> int64 = "%int64_add"
external sub : int64 -> int64 -> int64 = "%int64_sub"
external mul : int64 -> int64 -> int64 = "%int64_mul"
external div : int64 -> int64 -> int64 = "%int64_div"
external logor : int64 -> int64 -> int64 = "%int64_or"
external neg : int64 -> int64 = "%int64_neg"
external to_int : int64 -> int = "%int64_to_int"
  

(* external discard_sign : int64 -> int64 = "js_int64_discard_sign" *)
(* Same as {!Caml_int64.discard_sign} *)

external div_mod : int64 -> int64 -> int64 * int64 = "js_int64_div_mod"
(* Same as {!Caml_int64.div_mod} *)

external to_hex : int64 -> string = "js_int64_to_hex"    
(* same as {!Caml_int64.to_hex}*)
