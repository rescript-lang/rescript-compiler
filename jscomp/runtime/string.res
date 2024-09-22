// FIXME:
//   This exists for compatibility reason.
//   Move this into Pervasives or Core

type t = string
let length = Primitive_string_extern.length
let get = Primitive_string_extern.getChar
let unsafe_get = Primitive_string_extern.getChar
let compare = (x: t, y: t) => Pervasives.compare(x, y)
let equal: (string, string) => bool = (a, b) => a == b
