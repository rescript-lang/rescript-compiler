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

type t = 
  | Single of Lambda.let_kind  * Ident.t * Lambda.lambda
  | Recursive of (Ident.t * Lambda.lambda) list
  | Nop of Lambda.lambda 


val flatten : t list -> Lambda.lambda -> Lambda.lambda * t list

val lambda_of_groups : Lambda.lambda -> t list -> Lambda.lambda

val deep_flatten : Lambda.lambda -> Lambda.lambda
(** Tricky to be complete *)

val pp_group : Env.t -> Format.formatter -> t -> unit
