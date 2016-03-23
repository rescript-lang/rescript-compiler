



val caml_int64_float_of_bits : Caml_int64.t -> float
val caml_int32_float_of_bits : int32 -> float

val caml_int64_bits_of_float : float -> Caml_int64.t
val caml_int32_bits_of_float : float -> int32

val caml_classify_float : float -> fpclass
val caml_modf_float : float -> float * float 
val caml_ldexp_float : float -> int -> float 
val caml_frexp_float : float -> float * int
val caml_float_compare : float -> float -> int 
val caml_copysign_float : float -> float -> float
val caml_expm1_float : float -> float 

val caml_hypot_float : float -> float -> float  
val caml_log10_float : float -> float
