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

type binary_op =   ?comment:string -> t -> t -> t 

type unary_op =  ?comment:string -> t -> t

(** simplify 
    {[if b then ]}
    there is no need to convert b into OCaml boolean under this scenario
*)
val ocaml_boolean_under_condition : t -> t 





val var : ?comment:string  -> J.ident -> t 

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
val is_out : binary_op
val dot : ?comment:string -> t -> string -> t

val array_length : unary_op

val string_length : unary_op

val string_of_small_int_array : unary_op

val bytes_length :  unary_op

val function_length : unary_op

val char_of_int : unary_op

val char_to_int : unary_op

val array_append : binary_op

val array_copy : unary_op
val string_append : binary_op
(**
   When in ES6 mode, we can use Symbol to guarantee its uniquess,
   we can not tag [js] object, since it can be frozen 
*)



val var_dot : ?comment:string -> Ident.t -> string -> t
val bind_var_call : ?comment:string -> Ident.t -> string -> t list -> t 
val bind_call : ?comment:string -> J.expression -> string -> J.expression list -> t
val js_global_dot : ?comment:string -> string -> string -> t



val string_access : binary_op

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

val assign :  binary_op

val triple_equal : binary_op
(* TODO: reduce [triple_equal] use *)    

val float_equal : binary_op
val int_equal : binary_op
val string_equal : binary_op    
val is_type_number : unary_op
val typeof : unary_op

val to_int32 : unary_op
val to_uint32 : unary_op

val unchecked_int32_add : binary_op
val int32_add : binary_op
val unchecked_int32_minus : binary_op
val int32_minus : binary_op
val int32_mul : binary_op
val unchecked_int32_mul : binary_op

val int32_div : checked:bool -> binary_op
val int32_mod : checked:bool -> binary_op

val int32_lsl : binary_op
val int32_lsr : binary_op
val int32_asr : binary_op

val int32_bxor : binary_op
val int32_band : binary_op
val int32_bor : binary_op

val float_add : binary_op
val float_minus : binary_op
val float_mul : binary_op
val float_div : binary_op
val float_notequal : binary_op
val float_mod : binary_op  

val int_comp : Lambda.comparison -> binary_op
val string_comp : Js_op.binop -> binary_op
val float_comp :  Lambda.comparison -> binary_op
val js_comp :  Lambda.comparison -> binary_op


val not : t -> t

val call : ?comment:string  -> info:Js_call_info.t -> t -> t list -> t 

val flat_call : binary_op

val dump : ?comment:string -> Js_op.level -> t list -> t

(* val anything_to_string : unary_op *)

(** see {!https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Arithmetic_Operators#Unary_plus}*)
val to_number : unary_op
val int_to_string : unary_op
val to_json_string : unary_op

val new_ : ?comment:string -> J.expression -> J.expression list -> t

val array : 
  ?comment:string -> 
  J.mutable_flag -> 
  J.expression list ->
  t

val make_block : 
  ?comment:string ->
  J.expression -> (* tag *)
  J.tag_info ->  (* tag_info *)
  J.expression list -> 
  J.mutable_flag ->
  t


val seq : binary_op
val fuse_to_seq : t -> t list -> t 

val obj : ?comment:string -> J.property_map -> t 

val caml_true : t 

val caml_false : t

val bool : bool -> t



val unit :   t
(** [unit] in ocaml will be compiled into [0]  in js *)

(** [math "abs"] --> Math["abs"] *)    
val math : 
  ?comment:string -> 
  string -> 
  t list -> 
  t


val js_var : ?comment:string -> string -> t

val js_global : ?comment:string -> string -> t

val undefined : t
val is_caml_block : ?comment:string -> t -> t





val tag : ?comment:string -> J.expression -> t
val set_tag : ?comment:string -> J.expression -> J.expression -> t

(** Note that this is coupled with how we encode block, if we use the 
    `Object.defineProperty(..)` since the array already hold the length,
    this should be a nop 
*)

val set_length : ?comment:string -> J.expression -> J.expression -> t
val obj_length : ?comment:string -> J.expression -> t
val bool_of_boolean : unary_op

val and_ : binary_op
val or_ : binary_op

(** we don't expose a general interface, since a general interface is generally not safe *)
(* val is_instance_array  : unary_op *)

(** used combined with [caml_update_dummy]*)
val dummy_obj : ?comment:string ->  unit -> t 

(** convert a block to expresion by using IIFE *)    
val of_block : ?comment:string -> ?e:J.expression -> J.statement list -> t

val bind : binary_op

val raw_js_code : ?comment:string -> J.code_info ->  string -> t

val nil : t 
val is_nil : unary_op

val js_bool :  ?comment:string -> bool -> t 
val is_undef : unary_op
val for_sure_js_null_undefined_boolean : J.expression -> bool
val is_null_undefined : unary_op
val not_implemented : ?comment:string -> string -> t
