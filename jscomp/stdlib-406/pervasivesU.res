/* ************************************************************************ */
/*  */
/* OCaml */
/*  */
/* Xavier Leroy, projet Cristal, INRIA Rocquencourt */
/*  */
/* Copyright 1996 Institut National de Recherche en Informatique et */
/* en Automatique. */
/*  */
/* All rights reserved.  This file is distributed under the terms of */
/* the GNU Lesser General Public License version 2.1, with the */
/* special exception on linking described in the file LICENSE. */
/*  */
/* ************************************************************************ */

@@uncurried

module Jsx = JsxU
module JsxEvent = JsxEventU
module JsxDOM = JsxDOMU
module JsxPPXReactSupport = JsxPPXReactSupportU
module JsxModules = {
  module Jsx = JsxU
  module JsxEvent = JsxEventU
  module JsxDOM = JsxDOMU
}

/* Internal */
external __unsafe_cast: 'a => 'b = "%identity"

/* Exceptions */

external raise: exn => 'a = "%raise"
external raise_notrace: exn => 'a = "%raise_notrace"

let failwith = s => raise(Failure(s))
let invalid_arg = s => raise(Invalid_argument(s))

exception Exit

/* Composition operators */

external \"|>": ('a, 'a => 'b) => 'b = "%revapply"
external \"@@": ('a => 'b, 'a) => 'b = "%apply"

/* Debugging */

external __LOC__: string = "%loc_LOC"
external __FILE__: string = "%loc_FILE"
external __LINE__: int = "%loc_LINE"
external __MODULE__: string = "%loc_MODULE"
external __POS__: (string, int, int, int) = "%loc_POS"

external __LOC_OF__: 'a => (string, 'a) = "%loc_LOC"
external __LINE_OF__: 'a => (int, 'a) = "%loc_LINE"
external __POS_OF__: 'a => ((string, int, int, int), 'a) = "%loc_POS"

/* Comparisons */

external \"=": ('a, 'a) => bool = "%equal"
external \"<>": ('a, 'a) => bool = "%notequal"
external \"<": ('a, 'a) => bool = "%lessthan"
external \">": ('a, 'a) => bool = "%greaterthan"
external \"<=": ('a, 'a) => bool = "%lessequal"
external \">=": ('a, 'a) => bool = "%greaterequal"
external compare: ('a, 'a) => int = "%compare"
external min: ('a, 'a) => 'a = "%bs_min"
external max: ('a, 'a) => 'a = "%bs_max"
external \"==": ('a, 'a) => bool = "%eq"
external \"!=": ('a, 'a) => bool = "%noteq"

/* Boolean operations */

external not: bool => bool = "%boolnot"

external \"&&": (bool, bool) => bool = "%sequand"

external \"||": (bool, bool) => bool = "%sequor"

/* Integer operations */

external \"~-": int => int = "%negint"
external \"~+": int => int = "%identity"
external succ: int => int = "%succint"
external pred: int => int = "%predint"
external \"+": (int, int) => int = "%addint"
external \"-": (int, int) => int = "%subint"
external \"*": (int, int) => int = "%mulint"
external \"/": (int, int) => int = "%divint"
external mod: (int, int) => int = "%modint"

let abs = x =>
  if x >= 0 {
    x
  } else {
    -x
  }

external land: (int, int) => int = "%andint"
external lor: (int, int) => int = "%orint"
external lxor: (int, int) => int = "%xorint"

let lnot = x => lxor(x, -1)

external lsl: (int, int) => int = "%lslint"
external lsr: (int, int) => int = "%lsrint"
external asr: (int, int) => int = "%asrint"

let max_int = lsr(-1, 1)
let min_int = max_int + 1

/* Floating-point operations */

external \"~-.": float => float = "%negfloat"
external \"~+.": float => float = "%identity"
external \"+.": (float, float) => float = "%addfloat"
external \"-.": (float, float) => float = "%subfloat"
external \"*.": (float, float) => float = "%mulfloat"
external \"/.": (float, float) => float = "%divfloat"

@val @scope("Math") external \"**": (float, float) => float = "pow"
@val @scope("Math") external exp: float => float = "exp"
external expm1: float => float = "?expm1_float"

@val @scope("Math") external acos: float => float = "acos"
@val @scope("Math") external asin: float => float = "asin"
@val @scope("Math") external atan: float => float = "atan"
@val @scope("Math") external atan2: (float, float) => float = "atan2"
external hypot: (float, float) => float = "?hypot_float"

@val @scope("Math") external cos: float => float = "cos"
@val @scope("Math") external cosh: float => float = "cosh"
@val @scope("Math") external log: float => float = "log"
@val @scope("Math") external log10: float => float = "log10"
@val @scope("Math") external log1p: float => float = "log1p"
@val @scope("Math") external sin: float => float = "sin"
@val @scope("Math") external sinh: float => float = "sinh"
@val @scope("Math") external sqrt: float => float = "sqrt"
@val @scope("Math") external tan: float => float = "tan"
@val @scope("Math") external tanh: float => float = "tanh"
@val @scope("Math") external ceil: float => float = "ceil"
@val @scope("Math") external floor: float => float = "floor"
@val @scope("Math") external abs_float: float => float = "abs"
external copysign: (float, float) => float = "?copysign_float"
external mod_float: (float, float) => float = "?fmod_float"
external frexp: float => (float, int) = "?frexp_float"
external ldexp: (float, int) => float = "?ldexp_float"
external modf: float => (float, float) = "?modf_float"
external float: int => float = "%floatofint"
external float_of_int: int => float = "%floatofint"
external truncate: float => int = "%intoffloat"
external int_of_float: float => int = "%intoffloat"

let infinity = 0x1p2047
let neg_infinity = -0x1p2047
@val @scope("Number") external nan: float = "NaN"
let max_float = 1.79769313486231571e+308 /* 0x1.ffff_ffff_ffff_fp+1023 */
let min_float = 2.22507385850720138e-308 /* 0x1p-1022 */
let epsilon_float = 2.22044604925031308e-16 /* 0x1p-52 */

type fpclass =
  | FP_normal
  | FP_subnormal
  | FP_zero
  | FP_infinite
  | FP_nan

let classify_float = (x: float): fpclass =>
  if (%raw(`isFinite`): _ => _)(x) {
    if abs_float(x) >= /* 0x1p-1022 */ /* 2.22507385850720138e-308 */ min_float {
      FP_normal
    } else if x != 0. {
      FP_subnormal
    } else {
      FP_zero
    }
  } else if (%raw(`isNaN`): _ => _)(x) {
    FP_nan
  } else {
    FP_infinite
  }

/* String and byte sequence operations -- more in modules String and Bytes */

external string_length: string => int = "%string_length"

external \"^": (string, string) => string = "#string_append"
/* Character operations -- more in module Char */

external int_of_char: char => int = "%identity"
external unsafe_char_of_int: int => char = "%identity"
let char_of_int = n =>
  if n < 0 || n > 255 {
    invalid_arg("char_of_int")
  } else {
    unsafe_char_of_int(n)
  }

/* Unit operations */

external ignore: 'a => unit = "%ignore"

/* Pair operations */

external fst: (('a, 'b)) => 'a = "%field0"
external snd: (('a, 'b)) => 'b = "%field1"

/* References */

type ref<'a> = {mutable contents: 'a}
external ref: 'a => ref<'a> = "%makemutable"
external \"!": ref<'a> => 'a = "%bs_ref_field0"
external \":=": (ref<'a>, 'a) => unit = "%bs_ref_setfield0"
external incr: ref<int> => unit = "%incr"
external decr: ref<int> => unit = "%decr"

/* String conversion functions */
external format_float: (string, float) => string = "?format_float"

let string_of_bool = b =>
  if b {
    "true"
  } else {
    "false"
  }
let bool_of_string = param =>
  switch param {
  | "true" => true
  | "false" => false
  | _ => invalid_arg("bool_of_string")
  }

let bool_of_string_opt = param =>
  switch param {
  | "true" => Some(true)
  | "false" => Some(false)
  | _ => None
  }

@val external string_of_int: int => string = "String"

external int_of_string: string => int = "?int_of_string"

let int_of_string_opt = s =>
  /* TODO: provide this directly as a non-raising primitive. */
  try Some(int_of_string(s)) catch {
  | Failure(_) => None
  }

external string_get: (string, int) => char = "%string_safe_get"

let valid_float_lexem = s => {
  let l = string_length(s)
  let rec loop = i =>
    if i >= l {
      s ++ "."
    } else {
      switch string_get(s, i) {
      | '0' .. '9' | '-' => loop(i + 1)
      | _ => s
      }
    }

  loop(0)
}

let string_of_float = f => valid_float_lexem(format_float("%.12g", f))

external float_of_string: string => float = "?float_of_string"

let float_of_string_opt = s =>
  /* TODO: provide this directly as a non-raising primitive. */
  try Some(float_of_string(s)) catch {
  | Failure(_) => None
  }

/* List operations -- more in module List */

let rec \"@" = (l1, l2) =>
  switch l1 {
  | list{} => l2
  | list{hd, ...tl} => list{hd, ...\"@"(tl, l2)}
  }

/* Output functions on standard output */

@val @scope("console") external print_endline: string => unit = "log"
let print_newline = () => print_endline("")

/* Output functions on standard error */

@val @scope("console") external prerr_endline: string => unit = "error"
let prerr_newline = () => prerr_endline("")

let print_int = (i: int) => print_endline(string_of_int(i))
let print_float = (i: float) => print_endline(string_of_float(i))
let print_string = print_endline

/* Miscellaneous */

external sys_exit: int => 'a = "?sys_exit"

let exit_function = ref(ignore)

let at_exit = f => {
  let g = exit_function.contents
  exit_function :=
    () => {
      f()
      g()
    }
}

let do_at_exit = () => exit_function.contents()

let exit = retcode => {
  do_at_exit()
  sys_exit(retcode)
}

type int32 = int
