type shape

let init_mod: ((string, int, int), shape) => Obj.t

let update_mod: (shape, Obj.t, Obj.t) => unit
