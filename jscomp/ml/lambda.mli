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

type record_repr = 
  | Record_regular 
  | Record_optional

type tag_info = 
  | Blk_constructor of { name : string ; num_nonconst : int; tag : int; attrs : Parsetree.attributes }
  | Blk_record_inlined of { name : string ; num_nonconst :  int ;  tag : int; optional_labels: string list; fields : string array; mutable_flag : mutable_flag; attrs : Parsetree.attributes }   
  | Blk_tuple
  | Blk_poly_var of string 
  | Blk_record of {fields : string array; mutable_flag : mutable_flag; record_repr : record_repr }
  | Blk_module of string list
  | Blk_module_export of Ident.t list 
  | Blk_extension
    (* underlying is the same as tuple, immutable block
      {[
         exception A of int * int 
      ]}
      is translated into
      {[
        [A, x, y]
      ]}

    *)

  | Blk_some
  | Blk_some_not_nested (* ['a option] where ['a] can not inhabit a non-like value *)
  | Blk_record_ext of {fields : string array; mutable_flag : mutable_flag}
  | Blk_lazy_general    

val find_name :
  Parsetree.attribute -> Asttypes.label option

val tag_of_tag_info : tag_info -> int 
val mutable_flag_of_tag_info : tag_info -> mutable_flag 
val blk_record :    
  (Types.label_description* Typedtree.record_label_definition) array ->
  mutable_flag -> 
  record_repr -> 
  tag_info
  

val blk_record_ext :
  (Types.label_description* Typedtree.record_label_definition) array ->
  mutable_flag -> 
  tag_info


val blk_record_inlined :   
  (Types.label_description* Typedtree.record_label_definition) array ->
  string ->
  int ->
  string list ->
  tag:int ->
  attrs:Parsetree.attributes ->
  mutable_flag ->  
  tag_info




val ref_tag_info : tag_info

type field_dbg_info = 
  | Fld_record of {name : string; mutable_flag : Asttypes.mutable_flag}
  | Fld_module of {name : string}     
  | Fld_record_inline of {name : string}
  | Fld_record_extension of {name : string}
  | Fld_tuple
  | Fld_poly_var_tag
  | Fld_poly_var_content
  | Fld_extension
  | Fld_variant
  | Fld_cons
  | Fld_array
  
val fld_record :
  Types.label_description -> 
  field_dbg_info

val fld_record_inline :
  Types.label_description -> 
  field_dbg_info

val fld_record_extension :
  Types.label_description -> 
  field_dbg_info

val ref_field_info : field_dbg_info   



type set_field_dbg_info = 
  | Fld_record_set of string 
  | Fld_record_inline_set of string  
  | Fld_record_extension_set of string

val ref_field_set_info : set_field_dbg_info    

val fld_record_set : 
  Types.label_description ->
  set_field_dbg_info

val fld_record_inline_set : 
  Types.label_description ->
  set_field_dbg_info

val fld_record_extension_set :
  Types.label_description -> 
  set_field_dbg_info

type immediate_or_pointer =
  | Immediate
  | Pointer
type is_safe =
  | Safe
  | Unsafe

type pointer_info =
  | Pt_constructor of {name: string; const: int; non_const: int; attrs: Parsetree.attributes}
  | Pt_variant of {name: string}
  | Pt_module_alias
  | Pt_shape_none
  | Pt_assertfalse

type primitive =
  | Pidentity
  | Pbytes_to_string
  | Pignore
  | Prevapply
  | Pdirapply
  | Ploc of loc_kind
    (* Globals *)
  | Pgetglobal of Ident.t
  (* Operations on heap blocks *)
  | Pmakeblock of tag_info 
  | Pfield of int * field_dbg_info
  | Psetfield of int * set_field_dbg_info


  | Pduprecord
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
  (* BigInt operations *)
  | Pnegbigint | Paddbigint | Psubbigint | Ppowbigint
  | Pmulbigint | Pdivbigint | Pmodbigint
  | Pandbigint | Porbigint | Pxorbigint
  | Plslbigint | Pasrbigint
  | Pbigintcomp of comparison
  (* String operations *)
  | Pstringlength | Pstringrefu  | Pstringrefs
  | Pbyteslength | Pbytesrefu | Pbytessetu | Pbytesrefs | Pbytessets
  (* Array operations *)
  | Pmakearray of  mutable_flag
  | Parrayspread
  | Parraylength 
  | Parrayrefu 
  | Parraysetu 
  | Parrayrefs 
  | Parraysets 
  (* Test if the argument is a block or an immediate integer *)
  | Pisint
  (* Test if the (integer) argument is outside an interval *)
  | Pisout
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
  | Pctconst of compile_time_constant
  | Pcreate_extension of string
and comparison =
    Ceq | Cneq | Clt | Cgt | Cle | Cge


and value_kind =
    Pgenval 


and boxed_integer = Primitive.boxed_integer =
    Pbigint | Pint32 | Pint64


and raise_kind =
  | Raise_regular
  | Raise_reraise
  | Raise_notrace

type structured_constant =
    Const_base of constant
  | Const_pointer of int * pointer_info
  | Const_block of  tag_info * structured_constant list
  | Const_float_array of string list
  | Const_immstring of string
  | Const_false
  | Const_true
  
type inline_attribute =
  | Always_inline (* [@inline] or [@inline always] *)
  | Never_inline (* [@inline never] *)
  | Default_inline (* no [@inline] attribute *)




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




(* [true] means yes, [false] may mean unknown *)
type function_attribute = {
  inline : inline_attribute;
  is_a_functor: bool;
  return_unit : bool;
  async : bool;
  directive : string option;
  one_unit_arg : bool;
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
  | Lsend of string * lambda *  Location.t

and lfunction =
  {
    params: Ident.t list;
    body: lambda;
    attr: function_attribute; (* specified with [@inline] attribute *)
    loc : Location.t; }

and lambda_apply =
  { ap_func : lambda;
    ap_args : lambda list;
    ap_loc : Location.t;
    ap_inlined : inline_attribute; (* specified with the [@inlined] attribute *)
    }

and lambda_switch =
  { sw_numconsts: int;                  (* Number of integer cases *)
    sw_consts: (int * lambda) list;     (* Integer cases *)
    sw_numblocks: int;                  (* Number of tag block cases *)
    sw_blocks: (int * lambda) list;     (* Tag block cases *)
    sw_failaction : lambda option;      (* Action to take if failure *)
    sw_names: Ast_untagged_variants.switch_names option }



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
val lambda_assert_false: lambda
val lambda_unit: lambda
val lambda_module_alias : lambda
val name_lambda: let_kind -> lambda -> (Ident.t -> lambda) -> lambda
val name_lambda_list: lambda list -> (lambda list -> lambda) -> lambda

val iter: (lambda -> unit) -> lambda -> unit
module IdentSet: Set.S with type elt = Ident.t
val free_variables: lambda -> IdentSet.t

val transl_normal_path: Path.t -> lambda   (* Path.t is already normal *)

val transl_module_path: ?loc:Location.t -> Env.t -> Path.t -> lambda
val transl_value_path: ?loc:Location.t -> Env.t -> Path.t -> lambda
val transl_extension_path: ?loc:Location.t -> Env.t -> Path.t -> lambda


val make_sequence: ('a -> lambda) -> 'a list -> lambda

val subst_lambda: lambda Ident.tbl -> lambda -> lambda
val map : (lambda -> lambda) -> lambda -> lambda
val bind : let_kind -> Ident.t -> lambda -> lambda -> lambda

val commute_comparison : comparison -> comparison
val negate_comparison : comparison -> comparison

val default_function_attribute : function_attribute

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


val reset: unit -> unit
