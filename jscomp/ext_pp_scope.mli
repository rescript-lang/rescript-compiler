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



(** Scope type to improve identifier name printing
 *) 

(** Defines scope type [t], so that the pretty printer would print more beautiful code: 
    
    print [identifer] instead of [identifier$1234] when it can
 *)

type t 

val empty : t 

val add_ident : Ident.t -> t -> int * t

val sub_scope : t -> Ident_set.t -> t

val merge : Ident_set.t -> t -> t

val print : Format.formatter -> t -> unit
