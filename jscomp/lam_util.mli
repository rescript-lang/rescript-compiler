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



val string_of_lambda : Lambda.lambda -> string 

val string_of_primitive : Lambda.primitive -> string

val kind_of_lambda_block : Lambda.lambda list -> Lam_stats.kind

val get : Lambda.lambda -> Ident.t -> int -> Lam_stats.ident_tbl -> Lambda.lambda

val add_required_module : Ident.t -> Lam_stats.meta -> unit

val add_required_modules : Ident.t list -> Lam_stats.meta -> unit

val alias : Lam_stats.meta ->
  Ident.t -> Ident.t -> Lam_stats.kind -> Lambda.let_kind -> unit 


val refine_let : 
    ?kind:Lambda.let_kind ->
      Ident.t -> Lambda.lambda -> Lambda.lambda -> Lambda.lambda


val generate_label : ?name:string -> unit -> J.label

val sort_dag_args : J.expression Ident_map.t -> Ident.t list option
(** if [a] depends on [b] a is ahead of [b] as [a::b]

    TODO: make it a stable sort 
 *)

val dump : Env.t -> string ->  bool  -> Lambda.lambda -> Lambda.lambda

val ident_set_of_list : Ident.t list -> Lambda.IdentSet.t

val print_ident_set : Format.formatter -> Lambda.IdentSet.t -> unit

val mk_apply_info : ?loc:Location.t -> Lambda.apply_status -> Lambda.apply_info
