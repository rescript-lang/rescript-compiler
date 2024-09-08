external import: 'a => promise<'a> = "#import"

@@warning("-37") /* `Function` may be used in runtime */

type rec shape =
  | Function
  | Lazy
  | Class
  | Module(array<(shape, string)>)
  | Value(Obj.t)

@get external array_length: array<'a> => int = "length"
@get_index external array_unsafe_get: (array<'a>, int) => 'a = ""

@set_index external set_field: (Obj.t, string, Obj.t) => unit = ""
@get_index external get_field: (Obj.t, string) => Obj.t = ""

module type Empty = {}

/** Note that we have to provide a drop in replacement, since compiler internally will
    spit out ("CamlinternalMod".[init_mod|update_mod] unless we intercept it 
    in the lambda layer
*/
let init_mod = (loc: (string, int, int), shape: shape) => {
  let undef_module = _ => raise(Undefined_recursive_module(loc))
  let rec loop = (shape: shape, struct_: Obj.t, idx) =>
    switch shape {
    | Function => set_field(struct_, idx, Obj.magic(undef_module))
    | Lazy => set_field(struct_, idx, Obj.magic(undef_module))
    | Class => set_field(struct_, idx, Obj.magic((undef_module, undef_module, undef_module, 0)))
    | Module(comps) =>
      let v = Obj.repr(module({}: Empty))
      set_field(struct_, idx, v)
      let len = array_length(comps)
      for i in 0 to len - 1 {
        let (shape, name) = array_unsafe_get(comps, i)
        loop(shape, v, name)
      }
    | Value(v) => set_field(struct_, idx, v)
    }
  let res = Obj.repr(module({}: Empty))
  let dummy_name = "dummy"
  loop(shape, res, dummy_name)
  get_field(res, dummy_name)
}

/* Note the [shape] passed between [init_mod] and [update_mod] is always the same 
   and we assume [module] is encoded as an array
*/
let update_mod = (shape: shape, o: Obj.t, n: Obj.t): unit => {
  let rec aux = (shape: shape, o, n, parent, i) =>
    switch shape {
    | Function => set_field(parent, i, n)

    | Lazy
    | Class =>
      Caml_obj.update_dummy(o, n)
    | Module(comps) =>
      for i in 0 to array_length(comps) - 1 {
        let (shape, name) = array_unsafe_get(comps, i)
        aux(shape, get_field(o, name), get_field(n, name), o, name)
      }
    | Value(_) => ()
    }
  switch shape {
  | Module(comps) =>
    for i in 0 to array_length(comps) - 1 {
      let (shape, name) = array_unsafe_get(comps, i)
      aux(shape, get_field(o, name), get_field(n, name), o, name)
    }
  | _ => assert(false)
  }
}
