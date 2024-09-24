// FIXME:
//   This exists for compatibility reason.
//   Move this into Pervasives or Core

type t = Primitive_object_extern.t

@deprecated("Do not use directly. This will be removed in v13")
let repr = Primitive_object_extern.repr

@deprecated("Do not use directly. This will be removed in v13")
let obj = Primitive_object_extern.obj

@deprecated("Do not use directly. This will be removed in v13")
let magic = Primitive_object_extern.magic

@deprecated("Do not use directly. This will be removed in v13")
let tag = Primitive_object_extern.tag

@deprecated("Do not use directly. This will be removed in v13")
let size = Primitive_object_extern.size

@deprecated("Do not use directly. This will be removed in v13")
let field = Primitive_object_extern.getField

@deprecated("Do not use directly. This will be removed in v13")
let set_field = Primitive_object_extern.setField

@deprecated("Do not use directly. This will be removed in v13")
let dup = Primitive_object_extern.dup

@deprecated("Do not use directly. This will be removed in v13")
let update_dummy = Primitive_object_extern.updateDummy
