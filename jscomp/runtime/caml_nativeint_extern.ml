




external add : int -> int -> int = "nativeint_add"
external div : int -> int -> int = "nativeint_div"
external rem : int -> int -> int = "nativeint_mod"
external shift_right_logical : int -> int -> int = "nativeint_lsr"
external mul : int -> int -> int = "nativeint_mul"

external to_float : int -> float = "caml_nativeint_to_float"
external of_float : float -> int = "caml_nativeint_of_float"
external to_string : int -> string = "String" [@@bs.val]

