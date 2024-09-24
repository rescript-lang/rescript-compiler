type t

external repr: 'a => t = "%identity"
external obj: t => 'a = "%identity"
external magic: 'a => 'b = "%identity"

external tag: t => int = "%obj_tag"
external size: t => int = "%obj_size"
external field: (t, 'a) => t = "%obj_field"
external set_field: (t, 'a, t) => unit = "%obj_set_field"
external dup: t => t = "%obj_dup"

@scope("Object")
external update_dummy: (t, t) => unit = "assign"
