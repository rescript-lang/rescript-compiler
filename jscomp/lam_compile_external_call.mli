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



(** Compile ocaml external function call to JS IR. *) 

(** 
    This module define how the FFI (via `external`) works with attributes. 
    Note it will route to {!Lam_compile_global} 
    for compiling normal functions without attributes.
 *)


(** TODO: document supported attributes
    Attributes starting with `js` are reserved
    examples: "bs.splice"
 *)

val translate : 
  Lam_compile_defs.cxt -> 
  Types.type_expr option Primitive.description -> 
  J.expression list -> 
  J.expression
