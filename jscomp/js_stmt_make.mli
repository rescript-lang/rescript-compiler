(* BuckleScript compiler
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


type t = J.statement 

val mk :  ?comment:string  -> J.statement_desc -> t

val empty : ?comment:string  ->  unit -> t

val throw : ?comment:string  -> J.expression -> t

val if_ : 
  ?comment:string  ->
  ?declaration: Lambda.let_kind * Ident.t ->
  (* when it's not None, we also need make a variable declaration in the
     begininnig, however, we can optmize such case
  *)
  ?else_:J.block ->  
  J.expression -> 
  J.block -> 
  t

val block : ?comment:string  -> J.block -> t

val int_switch :
  ?comment:string -> ?declaration:Lambda.let_kind * Ident.t -> 
  ?default:J.block -> J.expression -> int J.case_clause list -> t 

val string_switch : ?comment:string -> ?declaration:Lambda.let_kind * Ident.t -> 
  ?default:J.block -> J.expression -> string J.case_clause list -> t

val declare_variable : ?comment:string ->
  ?ident_info:J.ident_info 
  -> kind:Lambda.let_kind -> Ident.t -> t

val define : 
  ?comment:string ->
  ?ident_info:J.ident_info ->
  kind:Lambda.let_kind -> Ident.t -> J.expression  -> t

val alias_variable :
  ?comment:string -> ?exp:J.expression -> Ident.t -> t
val assign : ?comment:string  -> J.ident -> J.expression -> t

val assign_unit : ?comment:string  -> J.ident -> t

val declare_unit : ?comment:string  -> J.ident -> t

val while_ : ?comment:string ->
  ?label:J.label -> ?env:Js_closure.t -> J.expression -> J.block -> t

val for_ : 
  ?comment:string ->
  ?env:Js_closure.t ->
  J.for_ident_expression option ->
  J.finish_ident_expression ->
  J.for_ident  -> J.for_direction -> J.block -> t

val try_ :
  ?comment:string  ->
  ?with_:J.ident * J.block -> ?finally:J.block -> J.block -> t

val exp : ?comment:string  -> J.expression -> t

val return : ?comment:string  -> J.expression -> t

val unknown_lambda : ?comment:string  -> Lambda.lambda -> t

val return_unit : ?comment:string -> unit -> t
(** for ocaml function which returns unit 
    it will be compiled into [return 0] in js *)

val break : ?comment:string  -> unit -> t

(** if [label] is not set, it will default to empty *)  
val continue : ?comment:string  -> ?label:J.label -> unit  -> t

