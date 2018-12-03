


external new_uninitialized : int -> bytes = "Array"  [@@bs.new]

external unsafe_get : bytes -> int -> char = "%bytes_unsafe_get"
external unsafe_set : bytes -> int -> char -> unit = "%bytes_unsafe_set"
external length : bytes -> int = "%bytes_length" 
