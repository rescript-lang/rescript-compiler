// Since [others] depend on this file, its public mli files **should not
// export types** introduced here, otherwise it would cause
// conflicts here.
//
// If the type exported here is also exported in modules from others,
// you will get a type not equivalent.
//
// Types defined here but should not export:
// - ref (make sure not exported in public others/*.mli)

external \"^": (string, string) => string = "#string_append"
external \"=": ('a, 'a) => bool = "%equal"
external \"<>": ('a, 'a) => bool = "%notequal"
external \"==": ('a, 'a) => bool = "%eq"
external \"!=": ('a, 'a) => bool = "%noteq"
external \"<": ('a, 'a) => bool = "%lessthan"
external \">": ('a, 'a) => bool = "%greaterthan"
external \"<=": ('a, 'a) => bool = "%lessequal"
external \">=": ('a, 'a) => bool = "%greaterequal"
external \"+": (int, int) => int = "%addint"
external \"-": (int, int) => int = "%subint"
external \"~-": int => int = "%negint"
external \"*": (int, int) => int = "%mulint"
external \"/": (int, int) => int = "%divint"
external lsl: (int, int) => int = "%lslint"
external lor: (int, int) => int = "%orint"
external land: (int, int) => int = "%andint"
external mod: (int, int) => int = "%modint"
external lsr: (int, int) => int = "%lsrint"
external lxor: (int, int) => int = "%xorint"
external asr: (int, int) => int = "%asrint"
type ref<'a> = {mutable contents: 'a}
external ref: 'a => ref<'a> = "%makemutable"

external \"||": (bool, bool) => bool = "%sequor"
external \"&&": (bool, bool) => bool = "%sequand"
external not: bool => bool = "%boolnot"

external raise: exn => 'a = "%raise"
external ignore: 'a => unit = "%ignore"
external \"|>": ('a, 'a => 'b) => 'b = "%revapply"
external \"@@": ('a => 'b, 'a) => 'b = "%apply"

@val @scope("Math") external \"**": (float, float) => float = "pow"
external \"~-.": float => float = "%negfloat"
external \"+.": (float, float) => float = "%addfloat"
external \"-.": (float, float) => float = "%subfloat"
external \"*.": (float, float) => float = "%mulfloat"
external \"/.": (float, float) => float = "%divfloat"

module Obj: {
  type t
  external field: (t, int) => t = "%obj_field"
  external set_field: (t, int, t) => unit = "%obj_set_field"
  external tag: t => int = "?obj_tag"
  external repr: 'a => t = "%identity"
  external obj: t => 'a = "%identity"
  external magic: 'a => 'b = "%identity"
  external size: t => int = "#obj_length"
}

module Pervasives: {
  external compare: ('a, 'a) => int = "%compare"
  external not: bool => bool = "%boolnot"
  external min: ('a, 'a) => 'a = "%bs_min"
  external max: ('a, 'a) => 'a = "%bs_max"
  external \"=": ('a, 'a) => bool = "%equal"
}
