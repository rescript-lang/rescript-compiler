(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)



type mutable_flag = Asttypes.mutable_flag

type ident = Ident.t

type function_kind 
   = Curried
   (* | Tupled *)


type primitive = 
  | Pbytes_to_string
  | Pbytes_of_string
  | Pglobal_exception of ident 
  | Pmakeblock of int * Lam_tag_info.t * Asttypes.mutable_flag
  | Pfield of int * Lambda.field_dbg_info
  | Psetfield of int  * Lambda.set_field_dbg_info
  | Pfloatfield of int * Lambda.field_dbg_info
  | Psetfloatfield of int * Lambda.set_field_dbg_info
  | Pduprecord of Types.record_representation * int
  | Plazyforce

  | Pccall of  Primitive_compat.t
  | Pjs_call of
    (* Location.t *  [loc] is passed down *)
    string *  (* prim_name *)
    External_arg_spec.t list * (* arg_types *)
    External_ffi_types.attr  (* ffi *)
  | Pjs_object_create of External_ffi_types.obj_create

  | Praise 
  | Psequand | Psequor | Pnot
  | Pnegint | Paddint | Psubint | Pmulint | Pdivint | Pmodint
  | Pandint | Porint | Pxorint
  | Plslint | Plsrint | Pasrint
  | Pintcomp of Lam_compat.comparison
  | Poffsetint of int
  | Poffsetref of int
  | Pintoffloat | Pfloatofint
  | Pnegfloat | Pabsfloat
  | Paddfloat | Psubfloat | Pmulfloat | Pdivfloat
  | Pfloatcomp of Lam_compat.comparison
  | Pjscomp of Lam_compat.comparison
  | Pjs_apply (*[f;arg0;arg1; arg2; ... argN]*)
  | Pjs_runtime_apply (* [f; [...]] *)
  | Pstringlength 
  | Pstringrefu 
  | Pstringrefs
  | Pstringadd    
  | Pbyteslength
  | Pbytesrefu
  | Pbytessetu 
  | Pbytesrefs
  | Pbytessets
  (* Array operations *)
  | Pmakearray of Lam_compat.array_kind
  | Parraylength 
  | Parrayrefu of Lam_compat.array_kind
  | Parraysetu of Lam_compat.array_kind
  | Parrayrefs of Lam_compat.array_kind
  | Parraysets of Lam_compat.array_kind
  (* Test if the argument is a block or an immediate integer *)
  | Pisint
  (* Test if the (integer) argument is outside an interval *)
  | Pisout
  (* Bitvect operations *)
  | Pbittest
  (* Operations on boxed integers (Nativeint.t, Int32.t, Int64.t) *)
  | Pbintofint of Lam_compat.boxed_integer
  | Pintofbint of Lam_compat.boxed_integer
  | Pcvtbint of Lam_compat.boxed_integer (*source*) * Lam_compat.boxed_integer (*destination*)
  | Pnegbint of Lam_compat.boxed_integer
  | Paddbint of Lam_compat.boxed_integer
  | Psubbint of Lam_compat.boxed_integer
  | Pmulbint of Lam_compat.boxed_integer
  | Pdivbint of Lam_compat.boxed_integer
  | Pmodbint of Lam_compat.boxed_integer
  | Pandbint of Lam_compat.boxed_integer
  | Porbint of Lam_compat.boxed_integer
  | Pxorbint of Lam_compat.boxed_integer
  | Plslbint of Lam_compat.boxed_integer
  | Plsrbint of Lam_compat.boxed_integer
  | Pasrbint of Lam_compat.boxed_integer
  | Pbintcomp of Lam_compat.boxed_integer * Lam_compat.comparison
  (* Operations on big arrays: (unsafe, #dimensions, kind, layout) *)
  | Pbigarrayref of bool * int * Lam_compat.bigarray_kind * Lam_compat.bigarray_layout
  | Pbigarrayset of bool * int * Lam_compat.bigarray_kind * Lam_compat.bigarray_layout
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
  | Pctconst of Lam_compat.compile_time_constant
  (* byte swap *)
  | Pbswap16
  | Pbbswap of Lam_compat.boxed_integer
  (* Integer to external pointer *)

  | Pdebugger
  | Pjs_unsafe_downgrade of string * Location.t
  | Pinit_mod
  | Pupdate_mod

  | Praw_js_code_exp of string 
  | Praw_js_code_stmt of string 
  | Praw_js_function of string * string list
  | Pjs_fn_make of int 
  | Pjs_fn_run of int 
  | Pjs_fn_method of int 
  | Pjs_fn_runmethod of int 
  | Pundefined_to_opt
  | Pnull_to_opt
  | Pnull_undefined_to_opt 
  
  | Pis_null
  | Pis_undefined
  | Pis_null_undefined

  | Pjs_typeof
  | Pjs_function_length 
  | Pcaml_obj_length
  | Pcaml_obj_set_length
  | Pwrap_exn (* convert either JS exception or OCaml exception into OCaml format *)  

  (* | Pcreate_exception of string  *)
  | Pcreate_extension of string 
  | Pis_not_none
  | Pval_from_option
  | Pval_from_option_not_nest
  | Psome
  | Psome_not_nest
type switch  =
  { sw_numconsts: int;
    sw_consts: (int * t) list;
    sw_numblocks: int;
    sw_blocks: (int * t) list;
    sw_failaction : t option}
and apply_status =
  | App_na
  | App_ml_full
  | App_js_full      
and apply_info = private
  { fn : t ; 
    args : t list ; 
    loc : Location.t;
    status : apply_status
  }

and prim_info = private
  { primitive : primitive ; 
    args : t list ; 
    loc : Location.t 
  }
and function_info = private
  { arity : int ; 
    function_kind : function_kind ; 
    params : ident list ;
    body : t 
  }
and  t =  private
  | Lvar of ident
  | Lglobal_module of ident
  | Lconst of Lam_constant.t
  | Lapply of apply_info
  | Lfunction of function_info
  | Llet of Lam_compat.let_kind * ident * t * t
  | Lletrec of (ident * t) list * t
  | Lprim of prim_info
  | Lswitch of t * switch
  | Lstringswitch of t * (string * t) list * t option
  | Lstaticraise of int * t list
  | Lstaticcatch of t * (int * ident list) * t
  | Ltrywith of t * ident * t
  | Lifthenelse of t * t * t
  | Lsequence of t * t
  | Lwhile of t * t
  | Lfor of ident * t * t * Asttypes.direction_flag * t
  | Lassign of ident * t
  | Lsend of Lambda.meth_kind * t * t * t list * Location.t
  | Lifused of ident * t
  (* | Levent of t * Lambda.lambda_event 
     [Levent] in the branch hurt pattern match, 
     we should use record for trivial debugger info
  *)





type binop = t -> t -> t 

type triop = t -> t -> t -> t 

type unop = t ->  t

val inner_map : (t -> t) -> t -> t
val inner_iter : (t -> unit) -> t -> unit 
val free_variables : t -> Ident_set.t

val no_bounded_variables : t -> bool 

val hit_any_variables : Ident_set.t -> t -> bool
val check : string -> t -> t 
type bindings = (Ident.t * t) list

val scc_bindings : bindings -> bindings list 
val scc : bindings -> t -> t  -> t 
val handle_bs_non_obj_ffi:
  External_arg_spec.t list ->
  External_ffi_types.return_wrapper ->
  External_ffi_types.attr -> t list -> Location.t -> string -> t
val exception_id_escaped : Ident.t -> t -> bool  

(**************************************************************)
(** Smart constructors *)
val var : ident -> t
val global_module : ident -> t 
val const : Lam_constant.t -> t

val apply : t -> t list -> Location.t -> apply_status -> t
val function_ : 
  arity:int ->
  function_kind:function_kind -> params:ident list -> body:t -> t

val let_ : Lam_compat.let_kind -> ident -> t -> t -> t
val letrec : (ident * t) list -> t -> t

(**  constant folding *)
val if_ : triop

(** constant folding*)
val switch : t -> switch  -> t 
(** constant folding*)
val stringswitch : t -> (string * t) list -> t option -> t 

val true_ : t 
val false_ : t 
val unit : t 

(** convert [l || r] to [if l then true else r]*)
val sequor : binop
(** convert [l && r] to [if l then r else false *)
val sequand : binop

(** constant folding *)
val not_ : Location.t ->  unop

(** drop unused block *)
val seq : binop
val while_ : binop
(* val event : t -> Lambda.lambda_event -> t   *)
val try_ : t -> ident -> t  -> t 
val ifused : ident -> t -> t
val assign : ident -> t -> t 

val send : 
  Lambda.meth_kind ->
  t -> t -> t list -> 
  Location.t -> t 

(** constant folding *)  
val prim : primitive:primitive -> args:t list -> Location.t  ->  t


val staticcatch : 
  t -> int * ident list -> t -> t

val staticraise : 
  int -> t list -> t

val for_ : 
  ident ->
  t  ->
  t -> Asttypes.direction_flag -> t -> t 


(**************************************************************)

