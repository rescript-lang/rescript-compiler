

external new_uninitialized : int -> 'a array = "Array" [@@bs.new]
external append : 'a array -> 'a array -> 'a array = "concat" [@@bs.send]
external unsafe_get : 'a array -> int -> 'a = "%array_unsafe_get"
external unsafe_set : 'a array -> int -> 'a -> unit = "%array_unsafe_set"
external length : 'a array -> int = "%array_length"

external caml_array_dup : 'a array -> (_ [@bs.as 0]) -> 'a array = 
  "slice"  [@@bs.send]

(* 
  Could be replaced by {!Caml_array.caml_make_vect}
  Leave here temporarily since we have marked it side effect free internally
*)
external make : int -> 'a -> 'a array = "caml_make_vect"    