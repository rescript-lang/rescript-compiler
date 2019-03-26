(* Copyright (C) 2018 Authors of BuckleScript
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

[@@@ocaml.warning "+9"]

type ident = Ident.t
  
type record_representation = Types.record_representation =  
    | Record_regular
    | Record_float
#if OCAML_VERSION =~ ">4.03.0" then
    | Record_unboxed of bool    (* Unboxed single-field record, inlined or not *)
    | Record_inlined of {tag : int; name : string; num_nonconsts : int}               (* Inlined record *)
    | Record_extension                    (* Inlined record under extension *)
#end  

type t =
  | Pbytes_to_string
  | Pbytes_of_string
  | Pglobal_exception of ident
  (* Operations on heap blocks *)
  | Pmakeblock of int * Lam_tag_info.t * Asttypes.mutable_flag
  | Pfield of int * Lam_compat.field_dbg_info
  | Psetfield of int * Lam_compat.set_field_dbg_info
  (* could have field info at least for record *)
  | Pfloatfield of int * Lam_compat.field_dbg_info
  | Psetfloatfield of int * Lam_compat.set_field_dbg_info
  | Pduprecord of record_representation * int
  (* Force lazy values *)
  | Plazyforce
  (* External call *)
  | Pccall of  Primitive_compat.t
  | Pjs_call of
      string *  (* prim_name *)
      External_arg_spec.t list * (* arg_types *)
      External_ffi_types.external_spec  (* ffi *)
  | Pjs_object_create of External_ffi_types.obj_create
  (* Exceptions *)
  | Praise
  (* Boolean operations *)
  | Psequand | Psequor | Pnot
  (* Integer operations *)
  | Pnegint | Paddint | Psubint | Pmulint | Pdivint | Pmodint
  | Pandint | Porint | Pxorint
  | Plslint | Plsrint | Pasrint
  | Pintcomp of Lam_compat.comparison
  | Poffsetint of int
  | Poffsetref of int
  (* Float operations *)
  | Pintoffloat | Pfloatofint
  | Pnegfloat 
  (* | Pabsfloat *) (* is {!Pervasives.abs_float} %abs_float*)
  | Paddfloat | Psubfloat | Pmulfloat | Pdivfloat
  | Pfloatcomp of Lam_compat.comparison
  | Pjscomp of Lam_compat.comparison
  | Pjs_apply (*[f;arg0;arg1; arg2; ... argN]*)
  | Pjs_runtime_apply (* [f; [...]] *)
  (* String operations *)
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
  | Parrayrefu
  | Parraysetu
  | Parrayrefs
  | Parraysets
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
  | Pwrap_exn (* convert either JS exception or OCaml exception into OCaml format *)

  (* | Pcreate_exception of string  *)
  | Pcreate_extension of string
  | Pis_not_none (* no info about its type *)
  | Pval_from_option
  | Pval_from_option_not_nest
  | Psome
  | Psome_not_nest  

  | Pfield_computed (* Mostly used in object compilation *)
  | Psetfield_computed




let eq_record_representation ( p : record_representation) ( p1 : record_representation) = 
  match p with 
  | Record_float -> p1 = Record_float
  | Record_regular -> p1 = Record_regular
#if OCAML_VERSION =~ ">4.03.0" then 
  | Record_unboxed b0 -> 
    (match p1 with 
    |Record_unboxed b1 -> b0 = b1 | _ -> false)
  | Record_inlined {tag = a0; name = a1; num_nonconsts = a2 } -> 
    (match p1 with 
    |Record_inlined {tag = b0; name = b1; num_nonconsts = b2} ->
       a0 = b0 && a1 = b1 && a2 = b2
    | _ -> false)
  | Record_extension -> 
    p1 = Record_extension   
#end
let eq_primitive_approx ( lhs : t) (rhs : t) = 
  match lhs with 
  | Pcreate_extension a -> begin match rhs with Pcreate_extension b -> a = (b : string) | _ -> false end
  | Pwrap_exn -> rhs = Pwrap_exn
  | Pbytes_to_string ->  rhs = Pbytes_to_string 
  | Pbytes_of_string ->  rhs = Pbytes_of_string
  | Praise -> rhs = Praise
  | Psequand -> rhs = Psequand
  | Psequor -> rhs = Psequor 
  | Pnot -> rhs = Pnot 
  | Pnegint -> rhs = Pnegint
  | Paddint -> rhs = Paddint 
  | Psubint -> rhs = Psubint
  | Pmulint -> rhs = Pmulint
  | Pdivint -> rhs = Pdivint
  | Pmodint -> rhs = Pmodint 
  | Pandint -> rhs = Pandint
  | Porint  -> rhs = Porint
  | Pxorint -> rhs = Pxorint
  | Plslint -> rhs = Plslint
  | Plsrint -> rhs = Plsrint
  | Pasrint -> rhs = Pasrint      
  | Pval_from_option -> rhs = Pval_from_option
  | Pval_from_option_not_nest -> rhs = Pval_from_option_not_nest
  | Plazyforce -> rhs = Plazyforce
  | Pintoffloat -> rhs = Pintoffloat
  | Pfloatofint -> rhs = Pfloatofint
  | Pnegfloat -> rhs =  Pnegfloat
  (* | Pabsfloat -> rhs = Pabsfloat *)
  | Paddfloat -> rhs = Paddfloat
  | Psubfloat -> rhs = Psubfloat
  | Pmulfloat -> rhs = Pmulfloat
  | Pdivfloat -> rhs = Pdivfloat
  | Pjs_apply -> rhs = Pjs_apply
  | Pjs_runtime_apply -> rhs = Pjs_runtime_apply
  | Pstringlength ->  rhs = Pstringlength
  | Pstringrefu ->  rhs = Pstringrefu
  | Pstringrefs ->  rhs = Pstringrefs
  | Pstringadd  ->  rhs = Pstringadd   
  | Pbyteslength -> rhs = Pbyteslength
  | Pbytesrefu ->   rhs = Pbytesrefu
  | Pbytessetu ->   rhs = Pbytessetu
  | Pbytesrefs ->   rhs = Pbytesrefs
  | Pbytessets ->   rhs = Pbytessets  
  | Pundefined_to_opt -> rhs = Pundefined_to_opt
  | Pnull_to_opt -> rhs = Pnull_to_opt
  | Pnull_undefined_to_opt -> rhs = Pnull_undefined_to_opt  
  | Pis_null -> rhs = Pis_null
  | Pis_not_none -> rhs = Pis_not_none 
  | Psome -> rhs = Psome
  | Psome_not_nest -> rhs = Psome_not_nest 
  | Pis_undefined -> rhs = Pis_undefined
  | Pis_null_undefined -> rhs = Pis_null_undefined
  | Pjs_typeof -> rhs = Pjs_typeof
  | Pisint -> rhs = Pisint
  | Pisout -> rhs = Pisout
  | Pbittest -> rhs = Pbittest
  | Pdebugger -> rhs = Pdebugger    
  | Pinit_mod -> rhs = Pinit_mod
  | Pupdate_mod -> rhs = Pupdate_mod
  | Pbswap16 -> rhs = Pbswap16
  | Pjs_function_length -> rhs = Pjs_function_length
  (* | Pjs_string_of_small_array -> rhs = Pjs_string_of_small_array *)
  (* | Pjs_is_instance_array -> rhs = Pjs_is_instance_array *)
  | Pcaml_obj_length -> rhs = Pcaml_obj_length
  (* | Pcaml_obj_set_length -> rhs = Pcaml_obj_set_length *)
  | Pccall {prim_name = n0 ;  prim_native_name = nn0; _} -> 
   (match rhs with 
   | Pccall {prim_name = n1; prim_native_name = nn1; _} 
    -> n0 = n1 && nn0 = nn1 
   | _ -> false )    
  | Pfield (n0, _dbg_info0) ->  (match rhs with Pfield (n1, _dbg_info1) ->  n0 = n1  | _ -> false )    
  | Psetfield(i0, _dbg_info0) -> (match rhs with Psetfield(i1, _dbg_info1) ->  i0 = i1  | _ -> false)
  | Pglobal_exception ident -> (match rhs with Pglobal_exception ident2 ->  Ident.same ident ident2 | _ -> false )
  | Pmakeblock (i, _tag_info, mutable_flag) -> (match rhs with Pmakeblock(i1,_,mutable_flag1) ->  i = i1 && mutable_flag = mutable_flag1  | _ -> false)
  | Pfloatfield (i0,_dbg_info) -> (match rhs with Pfloatfield (i1,_) -> i0 = i1   | _ -> false)
  | Psetfloatfield (i0,_dbg_info) ->  (match rhs with Psetfloatfield(i1,_) -> i0 = i1  | _ -> false)
  | Pduprecord (record_repesentation0,i1) -> (match rhs with Pduprecord(record_repesentation1,i2) ->  eq_record_representation record_repesentation0 record_repesentation1 && i1 = i2    | _ -> false)
  | Pjs_call (prim_name, arg_types, ffi) ->  ( match rhs with Pjs_call(prim_name1, arg_types1,ffi1) -> prim_name = prim_name1 && arg_types = arg_types1 && ffi = ffi1 | _ -> false)
  | Pjs_object_create obj_create -> (match rhs with Pjs_object_create obj_create1 -> obj_create = obj_create1 | _ -> false )
  | Pintcomp comparison -> (match rhs with Pintcomp comparison1 -> Lam_compat.eq_comparison comparison  comparison1  | _ -> false )    
  | Pfloatcomp comparison -> (match rhs with Pfloatcomp comparison1 -> Lam_compat.eq_comparison comparison  comparison1 | _ -> false)
  | Pjscomp comparison ->  (match rhs with  Pjscomp comparison1 -> Lam_compat.eq_comparison comparison  comparison1  | _ -> false )    
  | Poffsetint i0 ->   (match rhs with  Poffsetint i1 -> i0 = i1 | _ -> false )   
  | Poffsetref i0 ->  (match rhs with Poffsetref i1 -> i0 = i1   | _ -> false)
  | Pmakearray array_kind -> (match rhs with Pmakearray array_kind1 -> Lam_compat.eq_array_kind array_kind array_kind1 | _ -> false  )
  | Parraylength  -> rhs = Parraylength
  | Parrayrefu  -> rhs = Parrayrefu
  | Parraysetu  -> rhs = Parraysetu
  | Parrayrefs -> rhs = Parrayrefs
  | Parraysets -> rhs = Parraysets
  | Pbintofint  boxed_integer -> (match rhs with Pbintofint boxed_integer1 -> Lam_compat.eq_boxed_integer boxed_integer boxed_integer1 | _ -> false )
  | Pintofbint  boxed_integer -> (match rhs with Pintofbint boxed_integer1 -> Lam_compat.eq_boxed_integer boxed_integer boxed_integer1 | _ -> false )
  | Pnegbint  boxed_integer -> (match rhs with Pnegbint boxed_integer1 -> Lam_compat.eq_boxed_integer boxed_integer boxed_integer1 | _ -> false )
  | Paddbint  boxed_integer -> (match rhs with Paddbint boxed_integer1 -> Lam_compat.eq_boxed_integer boxed_integer boxed_integer1 | _ -> false )
  | Psubbint  boxed_integer -> (match rhs with Psubbint boxed_integer1 -> Lam_compat.eq_boxed_integer boxed_integer boxed_integer1 | _ -> false )
  | Pmulbint  boxed_integer -> (match rhs with Pmulbint boxed_integer1 -> Lam_compat.eq_boxed_integer boxed_integer boxed_integer1 | _ -> false )
  | Pdivbint  boxed_integer -> (match rhs with Pdivbint boxed_integer1 -> Lam_compat.eq_boxed_integer boxed_integer boxed_integer1 | _ -> false )
  | Pmodbint  boxed_integer -> (match rhs with Pmodbint boxed_integer1 -> Lam_compat.eq_boxed_integer boxed_integer boxed_integer1 | _ -> false )
  | Pandbint  boxed_integer -> (match rhs with Pandbint boxed_integer1 -> Lam_compat.eq_boxed_integer boxed_integer boxed_integer1 | _ -> false )
  | Porbint boxed_integer ->   (match rhs with Porbint  boxed_integer1 -> Lam_compat.eq_boxed_integer boxed_integer boxed_integer1 | _ -> false )
  | Pxorbint  boxed_integer -> (match rhs with Pxorbint boxed_integer1 -> Lam_compat.eq_boxed_integer boxed_integer boxed_integer1 | _ -> false )
  | Plslbint  boxed_integer -> (match rhs with Plslbint boxed_integer1 -> Lam_compat.eq_boxed_integer boxed_integer boxed_integer1 | _ -> false )
  | Plsrbint  boxed_integer -> (match rhs with Plsrbint boxed_integer1 -> Lam_compat.eq_boxed_integer boxed_integer boxed_integer1 | _ -> false )
  | Pasrbint  boxed_integer -> (match rhs with Pasrbint boxed_integer1 -> Lam_compat.eq_boxed_integer boxed_integer boxed_integer1 | _ -> false )
  | Pbbswap boxed_integer ->   (match rhs with Pbbswap boxed_integer1  -> Lam_compat.eq_boxed_integer boxed_integer boxed_integer1 | _ -> false )
  | Pcvtbint  (boxed_integer, boxed_integer1) -> (match rhs with Pcvtbint (boxed_integer10, boxed_integer11) -> Lam_compat.eq_boxed_integer boxed_integer boxed_integer10 && Lam_compat.eq_boxed_integer boxed_integer1 boxed_integer11 | _ -> false )
  | Pbintcomp  (boxed_integer , comparison) -> (match rhs with Pbintcomp(boxed_integer1, comparison1) -> Lam_compat.eq_boxed_integer boxed_integer boxed_integer1 && Lam_compat.eq_comparison comparison comparison1 | _ -> false)  
  | Pbigarraydim dim -> (match rhs with Pbigarraydim dim1 -> dim = dim1 | _ -> false )
  | Pstring_load_16 str ->  (match  rhs with Pstring_load_16 str1 -> str = str1  | _ -> false )
  | Pstring_load_32 b -> (match rhs with Pstring_load_32 b1 -> b = b1 | _ -> false )    
  | Pstring_load_64 b -> (match rhs with Pstring_load_64 b1 -> b = b1 | _ -> false )    
  | Pstring_set_16 b -> (match rhs with Pstring_set_16 b1 -> b = b1 | _ -> false )    
  | Pstring_set_32 b -> (match rhs with Pstring_set_32 b1 -> b = b1 | _ -> false )    
  | Pstring_set_64 b -> (match rhs with Pstring_set_64 b1 -> b = b1 | _ -> false )      
  | Pbigstring_load_16 b -> (match rhs with Pbigstring_load_16 b1 -> b = b1 | _ -> false )      
  | Pbigstring_load_32 b -> (match rhs with Pbigstring_load_32 b1 -> b = b1 | _ -> false )      
  | Pbigstring_load_64 b -> (match rhs with Pbigstring_load_64 b1 -> b = b1 | _ -> false )      
  | Pbigstring_set_16 b -> (match rhs with Pbigstring_set_16 b1 -> b = b1 | _ -> false )      
  | Pbigstring_set_32 b -> (match rhs with Pbigstring_set_32 b1 -> b = b1 | _ -> false )      
  | Pbigstring_set_64 b -> (match rhs with Pbigstring_set_64 b1 -> b = b1 | _ -> false )      
  | Pctconst compile_time_constant -> (match rhs with Pctconst compile_time_constant1 -> Lam_compat.eq_compile_time_constant compile_time_constant compile_time_constant1 | _ -> false)
  | Pjs_unsafe_downgrade ( s,_loc) -> (match rhs with Pjs_unsafe_downgrade (s1,_) -> s = s1 | _ -> false)  
  | Pjs_fn_make i -> (match rhs with Pjs_fn_make i1 -> i = i1 | _ -> false)
  | Pjs_fn_run i -> (match rhs with Pjs_fn_run i1 -> i = i1 | _ -> false)
  | Pjs_fn_method i -> (match rhs with Pjs_fn_method i1 -> i = i1 | _ ->  false )
  | Pjs_fn_runmethod i -> (match rhs with Pjs_fn_runmethod i1 -> i = i1 | _ -> false ) 

  | Pbigarrayref  _ 
  | Pbigarrayset _ 
  | Praw_js_function _
  | Praw_js_code_exp _ 
  | Praw_js_code_stmt _ -> false (* TOO lazy, here comparison is only approximation*)
  
  | Pfield_computed -> rhs = Pfield_computed
  | Psetfield_computed -> rhs = Psetfield_computed