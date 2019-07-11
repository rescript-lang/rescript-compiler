type extern_flags = No_sharing | Closures | Compat_32

external to_buffer_unsafe :
  bytes -> int -> int -> 'a -> extern_flags list -> int
  = "caml_output_value_to_buffer"

let to_buffer buff ofs len v flags =
  if ofs < 0 || len < 0 || ofs > Bytes.length buff - len then
    invalid_arg "Marshal.to_buffer: substring out of bounds"
  else to_buffer_unsafe buff ofs len v flags
