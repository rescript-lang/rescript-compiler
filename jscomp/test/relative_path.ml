(* include (struct *)
external foo : int = "foo" [@@bs.module "./File.js"]

(* end : sig val foo : int end ) *)

external foo2 : int -> int = "foo2" [@@bs.module "./File.js"]

let bar = foo
