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



(** Simplified wrapper module for the standard library [Format] module. 
  *) 

type t = private Format.formatter

val string : t -> string -> unit

val break : t -> unit

val break1 : t -> unit

val space :  t -> unit

val group : t -> int -> (unit -> 'a) -> 'a
(** [group] will record current indentation 
    and indent futher
 *)

val vgroup : t -> int -> (unit -> 'a) -> 'a

val paren : t -> (unit -> 'a) -> 'a

val paren_group : t -> int -> (unit -> 'a) -> 'a

val brace_group : t -> int -> (unit -> 'a) -> 'a

val brace_vgroup : t -> int -> (unit -> 'a) -> 'a

val bracket_group : t -> int -> (unit -> 'a) -> 'a

val newline : t -> unit

val to_out_channel : out_channel -> t

val flush : t -> unit -> unit
