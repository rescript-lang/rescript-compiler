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

val no_side_effects : Lambda.lambda -> bool 
(** No side effect, but it might depend on data store *)

val size : Lambda.lambda -> int

val eq_lambda : Lambda.lambda -> Lambda.lambda -> bool 
(** a conservative version of comparing two lambdas, mostly 
    for looking for similar cases in switch
 *)

val beta_reduce : Ident.t list -> Lambda.lambda -> Lambda.lambda list -> Lambda.lambda
(* Compile-time beta-reduction of functions immediately applied:
      Lapply(Lfunction(Curried, params, body), args, loc) ->
        let paramN = argN in ... let param1 = arg1 in body
      Lapply(Lfunction(Tupled, params, body), [Lprim(Pmakeblock(args))], loc) ->
        let paramN = argN in ... let param1 = arg1 in body
   Assumes |args| = |params|.
*)

val refine_let : 
    ?kind:Lambda.let_kind ->
      Ident.t -> Lambda.lambda -> Lambda.lambda -> Lambda.lambda

type group = 
  | Single of Lambda.let_kind  * Ident.t * Lambda.lambda
  | Recursive of (Ident.t * Lambda.lambda) list
  | Nop of Lambda.lambda 

val flatten : group list -> Lambda.lambda -> Lambda.lambda * group list

val lambda_of_groups : Lambda.lambda -> group list -> Lambda.lambda

val deep_flatten : Lambda.lambda -> Lambda.lambda
(** Tricky to be complete *)

val pp_group : Env.t -> Format.formatter -> group -> unit

val generate_label : ?name:string -> unit -> J.label 

val sort_dag_args : J.expression Ident_map.t -> Ident.t list option
(** if [a] depends on [b] a is ahead of [b] as [a::b]

    TODO: make it a stable sort 
 *)

val dump : Env.t -> string ->  bool  -> Lambda.lambda -> Lambda.lambda
