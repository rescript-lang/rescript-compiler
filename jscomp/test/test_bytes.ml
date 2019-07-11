let f v = Bytes.unsafe_to_string v

external uu : bytes -> string = "%bytes_to_string"

let ff v = uu v
