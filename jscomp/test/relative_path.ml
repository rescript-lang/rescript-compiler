
(* include (struct *)
external foo: int = ""
[@@bs.module "./File.js"]
(* end : sig val foo : int end ) *)

external foo2: int -> int = ""
[@@bs.module "./File.js"]

let bar = foo