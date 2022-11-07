(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* The "lambda" intermediate code *)

open Asttypes

type compile_time_constant =
  | Big_endian
  | Word_size
  | Int_size
  | Max_wosize
  | Ostype_unix
  | Ostype_win32
  | Ostype_cygwin
  | Backend_type

type loc_kind =
  | Loc_FILE
  | Loc_LINE
  | Loc_MODULE
  | Loc_LOC
  | Loc_POS

type immediate_or_pointer =
  | Immediate
  | Pointer

type initialization_or_assignment =
  | Assignment
  (* Initialization of in heap values, like [caml_initialize] C primitive.  The
     field should not have been read before and initialization should happen
     only once. *)
  | Heap_initialization
  (* Initialization of roots only. Compiles to a simple store.
     No checks are done to preserve GC invariants.  *)
  | Root_initialization

type is_safe =
  | Safe
  | Unsafe

type primitive =
  | Pidentity
  | Pbytes_to_string
  | Pbytes_of_string
  | Pignore
  | Prevapply
  | Pdirapply
  | Ploc of loc_kind
    (* Globals *)
  | Pgetglobal of Ident.t
  | Psetglobal of Ident.t
  (* Operations on heap blocks *)
  | Pmakeblock of int * mutable_flag * block_shape
  | Pfield of int
  | Pfield_computed
  | Psetfield of int * immediate_or_pointer * initialization_or_assignment
  | Psetfield_computed of immediate_or_pointer * initialization_or_assignment
  | Pfloatfield of int
  | Psetfloatfield of int * initialization_or_assignment
  | Pduprecord of Types.record_representation * int
  (* Force lazy values *)
  | Plazyforce
  (* External call *)
  | Pccall of Primitive.description
  (* Exceptions *)
  | Praise of raise_kind
  (* Boolean operations *)
  | Psequand | Psequor | Pnot
  (* Integer operations *)
  | Pnegint | Paddint | Psubint | Pmulint
  | Pdivint of is_safe | Pmodint of is_safe
  | Pandint | Porint | Pxorint
  | Plslint | Plsrint | Pasrint
  | Pintcomp of comparison
  | Poffsetint of int
  | Poffsetref of int
  (* Float operations *)
  | Pintoffloat | Pfloatofint
  | Pnegfloat | Pabsfloat
  | Paddfloat | Psubfloat | Pmulfloat | Pdivfloat
  | Pfloatcomp of comparison
  (* String operations *)
  | Pstringlength | Pstringrefu  | Pstringrefs
  | Pbyteslength | Pbytesrefu | Pbytessetu | Pbytesrefs | Pbytessets
  (* Array operations *)
  | Pmakearray of array_kind * mutable_flag
  | Pduparray of array_kind * mutable_flag
  (** For [Pduparray], the argument must be an immutable array.
      The arguments of [Pduparray] give the kind and mutability of the
      array being *produced* by the duplication. *)
  | Parraylength of array_kind
  | Parrayrefu of array_kind
  | Parraysetu of array_kind
  | Parrayrefs of array_kind
  | Parraysets of array_kind
  (* Test if the argument is a block or an immediate integer *)
  | Pisint
  (* Test if the (integer) argument is outside an interval *)
  | Pisout
  (* Bitvect operations *)
  | Pbittest
  (* Operations on boxed integers (Nativeint.t, Int32.t, Int64.t) *)
  | Pbintofint of boxed_integer
  | Pintofbint of boxed_integer
  | Pcvtbint of boxed_integer (*source*) * boxed_integer (*destination*)
  | Pnegbint of boxed_integer
  | Paddbint of boxed_integer
  | Psubbint of boxed_integer
  | Pmulbint of boxed_integer
  | Pdivbint of { size : boxed_integer; is_safe : is_safe }
  | Pmodbint of { size : boxed_integer; is_safe : is_safe }
  | Pandbint of boxed_integer
  | Porbint of boxed_integer
  | Pxorbint of boxed_integer
  | Plslbint of boxed_integer
  | Plsrbint of boxed_integer
  | Pasrbint of boxed_integer
  | Pbintcomp of boxed_integer * comparison
  (* Operations on big arrays: (unsafe, #dimensions, kind, layout) *)
  | Pbigarrayref of bool * int * bigarray_kind * bigarray_layout
  | Pbigarrayset of bool * int * bigarray_kind * bigarray_layout
  (* size of the nth dimension of a big array *)
  | Pbigarraydim of int
  (* load/set 16,32,64 bits from a string: (unsafe)*)
  | Pstring_load_16 of bool
  | Pstring_load_32 of bool
  | Pstring_load_64 of bool
  | Pstring_set_16 of bool
  | Pstring_set_32 of bool
  | Pstring_set_64 of bool
  (* load/set 16,32,64 bits from a
     (char, int8_unsigned_elt, c_layout) Bigarray.Array1.t : (unsafe) *)
  | Pbigstring_load_16 of bool
  | Pbigstring_load_32 of bool
  | Pbigstring_load_64 of bool
  | Pbigstring_set_16 of bool
  | Pbigstring_set_32 of bool
  | Pbigstring_set_64 of bool
  (* Compile time constants *)
  | Pctconst of compile_time_constant
  (* byte swap *)
  | Pbswap16
  | Pbbswap of boxed_integer
  (* Integer to external pointer *)
  | Pint_as_pointer
  (* Inhibition of optimisation *)
  | Popaque

and comparison =
    Ceq | Cneq | Clt | Cgt | Cle | Cge

and array_kind =
    Pgenarray | Paddrarray | Pintarray | Pfloatarray

and value_kind =
    Pgenval | Pfloatval | Pboxedintval of boxed_integer | Pintval

and block_shape =
  value_kind list option

and boxed_integer = Primitive.boxed_integer =
    Pnativeint | Pint32 | Pint64

and bigarray_kind =
    Pbigarray_unknown
  | Pbigarray_float32 | Pbigarray_float64
  | Pbigarray_sint8 | Pbigarray_uint8
  | Pbigarray_sint16 | Pbigarray_uint16
  | Pbigarray_int32 | Pbigarray_int64
  | Pbigarray_caml_int | Pbigarray_native_int
  | Pbigarray_complex32 | Pbigarray_complex64

and bigarray_layout =
    Pbigarray_unknown_layout
  | Pbigarray_c_layout
  | Pbigarray_fortran_layout

and raise_kind =
  | Raise_regular
  | Raise_reraise
  | Raise_notrace

type structured_constant =
    Const_base of constant
  | Const_pointer of int
  | Const_block of int * structured_constant list
  | Const_float_array of string list
  | Const_immstring of string

type inline_attribute =
  | Always_inline (* [@inline] or [@inline always] *)
  | Never_inline (* [@inline never] *)
  | Unroll of int (* [@unroll x] *)
  | Default_inline (* no [@inline] attribute *)

type specialise_attribute =
  | Always_specialise (* [@specialise] or [@specialise always] *)
  | Never_specialise (* [@specialise never] *)
  | Default_specialise (* no [@specialise] attribute *)

type function_kind = Curried | Tupled

type let_kind = Strict | Alias | StrictOpt | Variable
(* Meaning of kinds for let x = e in e':
    Strict: e may have side-effects; always evaluate e first
      (If e is a simple expression, e.g. a variable or constant,
       we may still substitute e'[x/e].)
    Alias: e is pure, we can substitute e'[x/e] if x has 0 or 1 occurrences
      in e'
    StrictOpt: e does not have side-effects, but depend on the store;
      we can discard e if x does not appear in e'
    Variable: the variable x is assigned later in e'
 *)

type meth_kind = Self | Public | Cached

type shared_code = (int * int) list     (* stack size -> code label *)

type function_attribute = {
  inline : inline_attribute;
  specialise : specialise_attribute;
  is_a_functor: bool;
  stub: bool;
}

type lambda =
    Lvar of Ident.t
  | Lconst of structured_constant
  | Lapply of lambda_apply
  | Lfunction of lfunction
  | Llet of let_kind * value_kind * Ident.t * lambda * lambda
  | Lletrec of (Ident.t * lambda) list * lambda
  | Lprim of primitive * lambda list * Location.t
  | Lswitch of lambda * lambda_switch * Location.t
(* switch on strings, clauses are sorted by string order,
   strings are pairwise distinct *)
  | Lstringswitch of
      lambda * (string * lambda) list * lambda option * Location.t
  | Lstaticraise of int * lambda list
  | Lstaticcatch of lambda * (int * Ident.t list) * lambda
  | Ltrywith of lambda * Ident.t * lambda
  | Lifthenelse of lambda * lambda * lambda
  | Lsequence of lambda * lambda
  | Lwhile of lambda * lambda
  | Lfor of Ident.t * lambda * lambda * direction_flag * lambda
  | Lassign of Ident.t * lambda
  | Lsend of meth_kind * lambda * lambda * lambda list * Location.t
  | Levent of lambda * lambda_event
  | Lifused of Ident.t * lambda

and lfunction =
  { kind: function_kind;
    params: Ident.t list;
    body: lambda;
    attr: function_attribute; (* specified with [@inline] attribute *)
    loc : Location.t; }

and lambda_apply =
  { ap_func : lambda;
    ap_args : lambda list;
    ap_loc : Location.t;
    ap_should_be_tailcall : bool;       (* true if [@tailcall] was specified *)
    ap_inlined : inline_attribute; (* specified with the [@inlined] attribute *)
    ap_specialised : specialise_attribute; }

and lambda_switch =
  { sw_numconsts: int;                  (* Number of integer cases *)
    sw_consts: (int * lambda) list;     (* Integer cases *)
    sw_numblocks: int;                  (* Number of tag block cases *)
    sw_blocks: (int * lambda) list;     (* Tag block cases *)
    sw_failaction : lambda option}      (* Action to take if failure *)
and lambda_event =
  { lev_loc: Location.t;
    lev_kind: lambda_event_kind;
    lev_repr: int ref option;
    lev_env: Env.summary }

and lambda_event_kind =
    Lev_before
  | Lev_after of Types.type_expr
  | Lev_function
  | Lev_pseudo
  | Lev_module_definition of Ident.t

type program =
  { module_ident : Ident.t;
    main_module_block_size : int;
    required_globals : Ident.Set.t;    (* Modules whose initializer side effects
                                          must occur before [code]. *)
    code : lambda }
(* Lambda code for the middle-end.
   * In the closure case the code is a sequence of assignments to a
     preallocated block of size [main_module_block_size] using
     (Setfield(Getglobal(module_ident))). The size is used to preallocate
     the block.
   * In the flambda case the code is an expression returning a block
     value of size [main_module_block_size]. The size is used to build
     the module root as an initialize_symbol
     Initialize_symbol(module_name, 0,
       [getfield 0; ...; getfield (main_module_block_size - 1)])
*)

(* Sharing key *)
val make_key: lambda -> lambda option

val const_unit: structured_constant
val lambda_unit: lambda
val name_lambda: let_kind -> lambda -> (Ident.t -> lambda) -> lambda
val name_lambda_list: lambda list -> (lambda list -> lambda) -> lambda

val iter: (lambda -> unit) -> lambda -> unit
module IdentSet: Set.S with type elt = Ident.t
val free_variables: lambda -> IdentSet.t
val free_methods: lambda -> IdentSet.t

val transl_normal_path: Path.t -> lambda   (* Path.t is already normal *)
val transl_path: ?loc:Location.t -> Env.t -> Path.t -> lambda
[@@ocaml.deprecated "use transl_{module,value,extension,class}_path instead"]

val transl_module_path: ?loc:Location.t -> Env.t -> Path.t -> lambda
val transl_value_path: ?loc:Location.t -> Env.t -> Path.t -> lambda
val transl_extension_path: ?loc:Location.t -> Env.t -> Path.t -> lambda
val transl_class_path: ?loc:Location.t -> Env.t -> Path.t -> lambda

val make_sequence: ('a -> lambda) -> 'a list -> lambda

val subst_lambda: lambda Ident.tbl -> lambda -> lambda
val map : (lambda -> lambda) -> lambda -> lambda
val bind : let_kind -> Ident.t -> lambda -> lambda -> lambda

val commute_comparison : comparison -> comparison
val negate_comparison : comparison -> comparison

val default_function_attribute : function_attribute
val default_stub_attribute : function_attribute

(***********************)
(* For static failures *)
(***********************)

(* Get a new static failure ident *)
val next_raise_count : unit -> int
val next_negative_raise_count : unit -> int
  (* Negative raise counts are used to compile 'match ... with
     exception x -> ...'.  This disabled some simplifications
     performed by the Simplif module that assume that static raises
     are in tail position in their handler. *)

val staticfail : lambda (* Anticipated static failure *)

(* Check anticipated failure, substitute its final value *)
val is_guarded: lambda -> bool
val patch_guarded : lambda -> lambda -> lambda

val raise_kind: raise_kind -> string
val lam_of_loc : loc_kind -> Location.t -> lambda

val merge_inline_attributes
   : inline_attribute
  -> inline_attribute
  -> inline_attribute option

val reset: unit -> unit
