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



(** Define type t used in JS IR to collect some meta data for a function, like its closures, etc 
  *)

type t 

val empty :  ?immutable_mask:bool array  -> int -> t

val is_tailcalled : t -> bool

val is_empty : t -> bool 

val set_bound :  t -> Ident_set.t -> unit

val get_bound : t -> Ident_set.t 

val set_lexical_scope : t -> Ident_set.t -> unit

val get_lexical_scope : t -> Ident_set.t

val to_string : t -> string

val mark_unused : t -> int -> unit 

val get_unused : t -> int -> bool

val get_mutable_params : Ident.t list -> t -> Ident.t list

val get_bound : t -> Ident_set.t

val get_length : t -> int
