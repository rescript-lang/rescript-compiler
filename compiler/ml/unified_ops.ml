open Btype
open Types
open Misc

(*
  Unified_ops is for specialization of some primitive operators.

  For example adding two values. We have `+` for ints, `+.` for floats, and `++` for strings.
  That because we don't use implicit type conversion or overloading.

  It is a fundamental property of the ReScript language, but at the same time it is far from the best DX we can think of, and it became a problem when introducing new primitives like bigint.

  See discussion: https://github.com/rescript-lang/rescript-compiler/issues/6525

  1. Type level translation

  2. IR level translation
*)

type args = (Asttypes.arg_label * Parsetree.expression) list
type targs = (Asttypes.arg_label * Typedtree.expression option) list

type specialized_type = {
  int: Path.t;
  bool: Path.t option;
  float: Path.t option;
  bigint: Path.t option;
  string: Path.t option;
}

let specialized_types = create_hashtable [||]

type specialized_primitive = {
  int: Lambda.primitive;
  bool: Lambda.primitive option;
  float: Lambda.primitive option;
  bigint: Lambda.primitive option;
  string: Lambda.primitive option;
}

let translate_type_application (env : Env.t) (funct : Parsetree.expression)
    (args : args) : (targs * type_expr) option =
  None

let translate_primitive_application (env : Env.t) (prim : Primitive.description)
    (args : args) : Lambda.primitive option =
  None
