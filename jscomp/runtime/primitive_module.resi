external import: 'a => promise<'a> = "%import"

type shape
let init_mod: ((string, int, int), shape) => Primitive_object.t
let update_mod: (shape, Primitive_object.t, Primitive_object.t) => unit
