(* OCamlScript compiler
 * Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* Author: Hongbo Zhang  *)



(** Creator utilities for the [J] module *) 

val prim : string 

val exceptions : string

val io : string

val oo : string

val sys : string

val lex_parse : string 

val obj_runtime : string

val array : string

val format : string

val string : string 

val float : string 

val no_side_effect : J.expression -> bool

val is_constant : J.expression -> bool
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

val extract_non_pure : J.expression -> J.expression option

module Exp : sig 
  type t = J.expression 

  val mk : ?comment:string -> J.expression_desc -> t

  val access : ?comment:string -> t -> t -> t 

  val string_access : ?comment:string -> t -> t -> t

  val var : ?comment:string  -> J.ident -> t 

  val runtime_var_dot : ?comment:string -> string -> string -> t

  val runtime_var_vid : string -> string -> J.vident

  val ml_var_dot : ?comment:string -> Ident.t -> string -> t

  val external_var_dot : ?comment:string -> Ident.t -> string -> string -> t

  val ml_var : ?comment:string -> Ident.t -> t

  val runtime_call : string -> string -> t list -> t

  val runtime_ref : string -> string -> t

  val str : ?pure:bool -> ?comment:string -> string -> t 

  val efun : ?comment:string ->
    ?immutable_mask:bool array -> J.ident list -> J.block -> t

  val econd : ?comment:string -> t -> t -> t -> t

  val int : ?comment:string -> ?c:char ->  int -> t 

  val float : ?comment:string -> string -> t

  val zero_float_lit : t 

  val dot : ?comment:string -> t -> string -> t

  val array_length : ?comment:string -> t -> t

  val string_length : ?comment:string -> t -> t

  val string_of_small_int_array : ?comment:string -> t -> t
  
  val bytes_length :  ?comment:string -> t -> t

  val function_length : ?comment:string -> t -> t      

  val char_of_int : ?comment:string -> t -> t

  val char_to_int : ?comment:string -> t -> t

  val array_append : ?comment:string -> t -> t list -> t

  val string_append : ?comment:string -> t -> t -> t
  (**
     When in ES6 mode, we can use Symbol to guarantee its uniquess,
     we can not tag [js] object, since it can be frozen 
   *)
  
  val tag_ml_obj : ?comment:string -> t -> t

  val var_dot : ?comment:string -> Ident.t -> string -> t

  val js_global_dot : ?comment:string -> string -> string -> t

  val index : ?comment:string -> t -> int -> t

  val assign :  ?comment:string -> t -> t -> t 

  val triple_equal : ?comment:string -> t -> t -> t 

  val is_type_number : ?comment:string -> t -> t

  val bin : ?comment:string -> Js_op.binop -> t -> t -> t 

  val int_plus : ?comment:string -> t -> t -> t

  val int_minus : ?comment:string -> t -> t -> t

  val float_plus : ?comment:string -> t -> t -> t

  val float_minus : ?comment:string -> t -> t -> t
  (* val un : ?comment:string -> Js_op.unop -> t -> t  *)
  val not : t -> t

  val call : ?comment:string  -> ?info:Js_call_info.t -> t -> t list -> t 

  val flat_call : ?comment:string -> t -> t  -> t

  val dump : ?comment:string -> Js_op.level -> t list -> t

  val new_ : ?comment:string -> J.expression -> J.expression list -> t

  val arr : ?comment:string -> J.mutable_flag -> J.expression list -> t

  val uninitialized_array : ?comment:string -> t -> t

  val seq : ?comment:string -> t -> t -> t  

  val obj : ?comment:string -> J.property_map -> t 
  
  val true_ : t 

  val false_ : t

  val bool : bool -> t

  val unknown_lambda : ?comment:string -> Lambda.lambda -> t

  val unknown_primitive : ?comment:string -> Lambda.primitive -> t
  
  val unit :  unit -> t
  (** [unit] in ocaml will be compiled into [0]  in js *)

  val js_var : ?comment:string -> string -> t

  val js_global : ?comment:string -> string -> t

  val undefined : ?comment:string -> unit -> t

  val math : ?comment:string -> string -> t list -> t
  (** [math "abs"] --> Math["abs"] *)    

  val inc : ?comment:string -> t -> t

  val dec : ?comment:string -> t -> t
  
  val prefix_inc : ?comment:string -> J.vident -> t

  val prefix_dec : ?comment:string -> J.vident -> t
  
  val null : ?comment:string -> unit -> t
  
  val tag : ?comment:string -> J.expression -> t
  
  val to_ocaml_boolean : ?comment:string -> t -> t
  
  val and_ : ?comment:string -> t -> t -> t
  
  val or_ : ?comment:string -> t -> t -> t
      
  val lt : ?comment:string -> t -> t -> t    
      
  val le : ?comment:string -> t -> t -> t
      
  val gt : ?comment:string -> t -> t -> t    

  val ge : ?comment:string -> t -> t -> t

  val intcomp : ?comment:string -> Lambda.comparison -> t -> t -> t    

  val stringcomp : ?comment:string -> Js_op.binop -> t -> t -> t

  val add : ?comment:string -> t -> t -> t

  val minus : ?comment:string -> t -> t -> t

  val mul : ?comment:string -> t -> t -> t
  
  val div : ?comment:string -> t -> t -> t

  val of_block : ?comment:string -> J.statement list -> J.expression -> t
end

module Stmt : sig
  
  type t = J.statement 

  val mk :  ?comment:string  -> J.statement_desc -> t

  val empty : ?comment:string  ->  unit -> t

  val throw : ?comment:string  -> J.expression -> t

  val if_ : 
    ?comment:string  ->
    ?declaration: Lambda.let_kind * Ident.t -> (* when it's not None, 
                                                  we also need make a variable declaration in the
                                                  begininnig, however, we can optmize such case
                                                *)
    ?else_:J.block ->  
    J.expression -> 
    J.block -> 
    t

  val block : ?comment:string  -> J.block -> t

  val int_switch : ?comment:string -> ?declaration:Lambda.let_kind * Ident.t -> 
    ?default:J.block -> J.expression -> int J.case_clause list -> t 

  val string_switch : ?comment:string -> ?declaration:Lambda.let_kind * Ident.t -> 
    ?default:J.block -> J.expression -> string J.case_clause list -> t

  val declare_variable : ?comment:string ->
    ?ident_info:J.ident_info 
    -> kind:Lambda.let_kind -> Ident.t -> t

  val define : ?comment:string ->
    ?ident_info:J.ident_info ->
      kind:Lambda.let_kind -> Ident.t -> J.expression  -> t

  val const_variable :
      ?comment:string -> ?exp:J.expression -> Ident.t -> t
  val assign : ?comment:string  -> J.ident -> J.expression -> t

  val assign_unit : ?comment:string  -> J.ident -> t

  val declare_unit : ?comment:string  -> J.ident -> t

  val while_ : ?comment:string ->
    ?label:J.label -> ?env:Js_closure.t -> Exp.t -> J.block -> t

  val for_ : ?comment:string ->
    ?env:Js_closure.t ->
      J.for_ident_expression option ->
        J.finish_ident_expression ->
          J.for_ident  -> J.for_direction -> J.block -> t

  val try_ : ?comment:string  ->
    ?with_:J.ident * J.block -> ?finally:J.block -> J.block -> t

  val exp : ?comment:string  -> J.expression -> t

  val return : ?comment:string  -> J.expression -> t

  val unknown_lambda : ?comment:string  -> Lambda.lambda -> t

  val return_unit : ?comment:string -> unit -> t
  (** for ocaml function which returns unit 
      it will be compiled into [return 0] in js *)

  val break : ?comment:string  -> unit -> t

  val continue : ?comment:string  -> J.label -> t
end

