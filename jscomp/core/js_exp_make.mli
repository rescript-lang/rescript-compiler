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








(** Creator utilities for the [J] module *) 






(** check if a javascript ast is constant 

    The better signature might be 
    {[
      J.expresssion -> Js_output.t
    ]}
    for exmaple
    {[
      e ?print_int(3) :  0
                         --->
                         if(e){print_int(3)}
    ]}
*)
type t = J.expression 

val remove_pure_sub_exp : t -> t option

val var : ?comment:string  -> J.ident -> t 

val js_global : ?comment:string -> string -> t

(* val runtime_var_dot : ?comment:string -> string -> string -> t *)

(* val runtime_var_vid : string -> string -> J.vident *)

(** [ml_var_dot ocaml_module name]
*)
val ml_var_dot : ?comment:string -> Ident.t -> string -> t

(** [external_var_dot ~external_name ~dot id]
  Used in FFI
*)
val external_var_dot : 
  ?comment:string ->  
  external_name:string -> 
  ?dot:string -> 
  Ident.t ->
  t

val runtime_call : 
  ?comment:string -> 
  string -> (* module_name *)
  string -> (* fn_name *)
  t list -> (* args *)
  t

val runtime_ref : 
  string -> 
  string -> 
  t  

val public_method_call : string -> t -> t -> Int32.t -> t list -> t


val str : 
  ?pure:bool -> 
  ?comment:string -> 
  string -> 
  t 

val unicode : 
  ?comment:string -> 
  string -> 
  t
  
val ocaml_fun : ?comment:string ->
  ?immutable_mask:bool array -> J.ident list -> J.block -> t

val method_ : ?comment:string ->
  ?immutable_mask:bool array -> J.ident list -> J.block -> t

val econd : ?comment:string -> t -> t -> t -> t

val int : ?comment:string -> ?c:char ->  int32 -> t 
val nint : ?comment:string -> nativeint -> t 
val small_int : int -> t
val float : ?comment:string -> string -> t

val empty_string_literal : t 
(* TODO: we can do hash consing for small integers *)
val zero_int_literal : t
val one_int_literal : t
val zero_float_lit : t 
val obj_int_tag_literal : t

(** [is_out e range] is equivalent to [e > range or e <0]

*)
val is_out : ?comment:string -> t -> t -> t

val dot : ?comment:string -> t -> string -> t

val array_length : ?comment:string -> t -> t 

val string_length : ?comment:string -> t -> t 

val bytes_length :  ?comment:string -> t -> t 

val function_length : ?comment:string -> t -> t 

val char_of_int : ?comment:string -> t -> t 

val char_to_int : ?comment:string -> t -> t 

val string_append : ?comment:string -> t -> t -> t 
(**
   When in ES6 mode, we can use Symbol to guarantee its uniquess,
   we can not tag [js] object, since it can be frozen 
*)



(* val var_dot : ?comment:string -> Ident.t -> string -> t *)

(* val bind_var_call : ?comment:string -> Ident.t -> string -> t list -> t  *)

(* val bind_call : ?comment:string -> J.expression -> string -> J.expression list -> t *)
val js_global_dot : ?comment:string -> string -> string -> t



val string_access : ?comment:string -> t -> t -> t 

val access : 
  ?comment:string -> 
  t -> 
  t ->
  t
val index : 
  ?comment:string -> 
  t -> 
  Int32.t ->
   t

(** 
    [assign_addr  e i v]
    if the expression [e] is a temporay block 
    which has no side effect,
    write to it does not really make sense, 
    optimize it away *)
val assign_addr : 
  ?comment:string -> 
  t -> 
  Js_op.jsint -> 
  assigned_value:t -> 
  t

val assign :  ?comment:string -> t -> t -> t 

val triple_equal : ?comment:string -> t -> t -> t 
(* TODO: reduce [triple_equal] use *)    

val float_equal : ?comment:string -> t -> t -> t 
val int_equal : ?comment:string -> t -> t -> t 
val string_equal : ?comment:string -> t -> t -> t 
val eq_null_undefined_boolean: ?comment:string -> t -> t -> t 
val neq_null_undefined_boolean: ?comment:string -> t -> t -> t 
val is_type_number : ?comment:string -> t -> t 
val typeof : ?comment:string -> t -> t 

val to_int32 : ?comment:string -> t -> t 
val to_uint32 : ?comment:string -> t -> t 

val unchecked_int32_add : ?comment:string -> t -> t -> t 
val int32_add : ?comment:string -> t -> t -> t 
val unchecked_int32_minus : ?comment:string -> t -> t -> t 
val int32_minus : ?comment:string -> t -> t -> t 
val int32_mul : ?comment:string -> t -> t -> t 
val unchecked_int32_mul : ?comment:string -> t -> t -> t 

val int32_div : checked:bool -> ?comment:string -> t -> t -> t 
val int32_mod : checked:bool -> ?comment:string -> t -> t -> t 

val int32_lsl : ?comment:string -> t -> t -> t 
val int32_lsr : ?comment:string -> t -> t -> t 
val int32_asr : ?comment:string -> t -> t -> t 

val int32_bxor : ?comment:string -> t -> t -> t 
val int32_band : ?comment:string -> t -> t -> t 
val int32_bor : ?comment:string -> t -> t -> t 

val float_add : ?comment:string -> t -> t -> t 
val float_minus : ?comment:string -> t -> t -> t 
val float_mul : ?comment:string -> t -> t -> t 
val float_div : ?comment:string -> t -> t -> t 
val float_notequal : ?comment:string -> t -> t -> t 
val float_mod : ?comment:string -> t -> t -> t 

val int_comp : Lam_compat.comparison -> ?comment:string -> t -> t -> t 
val bool_comp : Lam_compat.comparison -> ?comment:string -> t -> t -> t 
val string_comp : Js_op.binop -> ?comment:string -> t -> t -> t 
val float_comp :  Lam_compat.comparison -> ?comment:string -> t -> t -> t 
val js_comp :  Lam_compat.comparison -> ?comment:string -> t -> t -> t 


val not : t -> t

val call : ?comment:string  -> info:Js_call_info.t -> t -> t list -> t 

val flat_call : ?comment:string -> t -> t -> t 

val new_ : ?comment:string -> J.expression -> J.expression list -> t

val array : 
  ?comment:string -> 
  J.mutable_flag -> 
  J.expression list ->
  t

val optional_block :
  J.expression -> 
  J.expression 
  
val optional_not_nest_block : 
  J.expression -> 
  J.expression  

val make_block : 
  ?comment:string ->
  J.expression -> (* tag *)
  J.tag_info ->  (* tag_info *)
  J.expression list -> 
  J.mutable_flag ->
  t


val seq : ?comment:string -> t -> t -> t 
val fuse_to_seq : t -> t list -> t 

val obj : 
  ?comment:string -> 
  J.property_map -> 
  t 

val true_ : t 

val false_ : t

val bool : bool -> t



val unit :   t
(** [unit] in ocaml will be compiled into [0]  in js *)

(** [math "abs"] --> Math["abs"] *)    
val math : 
  ?comment:string -> 
  string -> 
  t list -> 
  t



val undefined : t
val is_caml_block : ?comment:string -> t -> t


val tag : ?comment:string -> J.expression -> t
val block_set_tag : ?comment:string -> J.expression -> J.expression -> t

(** Note that this is coupled with how we encode block, if we use the 
    `Object.defineProperty(..)` since the array already hold the length,
    this should be a nop 
*)


val obj_length : ?comment:string -> J.expression -> t


val and_ : ?comment:string -> t -> t -> t 
val or_ : ?comment:string -> t -> t -> t 

(** we don't expose a general interface, since a general interface is generally not safe *)

(** used combined with [caml_update_dummy]*)
val dummy_obj : ?comment:string ->  unit -> t 

(** convert a block to expresion by using IIFE *)    
val of_block : ?comment:string -> ?e:J.expression -> J.statement list -> t

val raw_js_code : ?comment:string -> J.code_info ->  string -> t
val raw_js_function : ?comment:string -> string -> string list -> t
val nil : t 
val is_null : ?comment:string -> t -> t 


val is_undef : ?comment:string -> t -> t 
val for_sure_js_null_undefined : J.expression -> bool
val is_null_undefined : ?comment:string -> t -> t 
val not_implemented : ?comment:string -> string -> t
