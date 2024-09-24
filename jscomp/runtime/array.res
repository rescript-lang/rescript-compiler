// FIXME:
//   This exists for compatibility reason.
//   Move this into Pervasives or Core

let length = Primitive_array_extern.length
let unsafe_get = Primitive_array_extern.getUnsafe

// FIXME:
//   This module is implicitly used by
//   `array[idx]` syntax

external get: (array<'a>, int) => 'a = "%array_safe_get"
external set: (array<'a>, int, 'a) => unit = "%array_safe_set"
