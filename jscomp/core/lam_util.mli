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








val string_of_lambda : Lam.t -> string 

val string_of_primitive : Lam.primitive -> string

val kind_of_lambda_block : Lam_id_kind.boxed_nullable -> Lam.t list -> Lam_id_kind.t

val field_flatten_get : 
  (unit -> Lam.t) -> Ident.t -> int -> Lam_stats.ident_tbl -> Lam.t





val alias_ident_or_global : Lam_stats.t ->
  Ident.t -> Ident.t -> Lam_id_kind.t -> Lam.let_kind -> unit 


val refine_let : 
    kind:Lam.let_kind  ->
      Ident.t -> Lam.t -> Lam.t -> Lam.t


val generate_label : ?name:string -> unit -> J.label

(* val sort_dag_args : J.expression Ident_map.t -> Ident.t list option *)
(** if [a] depends on [b] a is ahead of [b] as [a::b]

    TODO: make it a stable sort 
 *)


(** [dump] when {!Js_config.is_same_file}*)
val dump : Env.t   -> string -> Lam.t -> Lam.t


val print_ident_set : Format.formatter -> Ident_set.t -> unit



val not_function : Lam.t -> bool 
val is_function : Lam.t -> bool 




val subst_lambda : Lam.t Ident_map.t -> Lam.t -> Lam.t



