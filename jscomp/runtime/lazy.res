// FIXME:
//   This exists for compatibility reason.
//   Move this into Pervasives or Core

type t<'a> = lazy_t<'a>

exception Undefined = Primitive_lazy.Undefined

let force = Primitive_lazy.force

let force_val = Primitive_lazy.force_val

let from_fun = Primitive_lazy.from_fun

let from_val = Primitive_lazy.from_val

let is_val = Primitive_lazy.is_val
