@new external new_uninitialized: int => array<'a> = "Array"
@send external append: (array<'a>, array<'a>) => array<'a> = "concat"
external unsafe_get: (array<'a>, int) => 'a = "%array_unsafe_get"
external unsafe_set: (array<'a>, int, 'a) => unit = "%array_unsafe_set"
external length: array<'a> => int = "%array_length"

/*
  Could be replaced by {!Caml_array.caml_make_vect}
  Leave here temporarily since we have marked it side effect free internally
*/
external make: (int, 'a) => array<'a> = "?make_vect"
