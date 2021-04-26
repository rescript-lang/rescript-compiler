

external add : int -> int -> int = "%int32_add"
external shift_left : int -> int -> int = "%int32_lsl"
external shift_right_logical : int -> int -> int = "%int32_lsr"
external shift_right : int -> int -> int = "%int32_asr"
external logand : int -> int -> int = "%int32_and"
external logxor : int -> int -> int = "%int32_xor"
external logor : int -> int -> int = "%int32_or"
external of_int : int -> int = "%int32_of_int"
external mul : int -> int -> int = "%int32_mul"

module Ops = struct 
  external (+~) : int -> int -> int = "%int32_add"
  external (<<~) : int -> int -> int = "%int32_lsl"
  external (>>>~) : int -> int -> int = "%int32_lsr"
  external (>>~) : int -> int -> int = "%int32_asr"
  external (&~) : int -> int -> int = "%int32_and"
  external (^~) : int -> int -> int = "%int32_xor"
  external (|~) : int -> int -> int = "%int32_or"
  external ( *~ ) : int -> int -> int = "%int32_mul"
end 