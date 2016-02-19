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




(** This pass is used to collect meta data information.

    It includes:
    alias table, arity for identifiers and might more information,
    
    ATTENTION:
    For later pass to keep its information complete and up to date,
    we  need update its table accordingly

    - Alias inference is not for substitution, it is for analyze which module is 
      actually a global module or an exception, so it can be relaxed a bit
      (without relying on strict analysis)

    - Js object (local) analysis 

    Design choice:

    Side effectful operations:
       - Lassign 
       - Psetfield

    1. What information should be collected:

    2. What's the key
       If it's identifier, 
       
    Information that is always sound, not subject to change 

    - shall we collect that if an identifier is passed as a parameter, (useful for escape analysis), 
    however, since it's going to change after inlning (for local function)

    - function arity, subject to change when you make it a mutable ref and change it later
    
    - Immutable blocks of identifiers
     
      if identifier itself is function/non block then the access can be inlined 
      if identifier itself is immutable block can be inlined
      if identifier is mutable block can be inlined (without Lassign) since

    - When collect some information, shall we propogate this information to 
      all alias table immeidately

      - annotation identifiers (at first time)
      -
 *)

(** Modify existing [meta] *)
val collect_helper : Lam_stats.meta -> Lambda.lambda -> unit

(** return a new [meta] *)
val count_alias_globals : 
    Env.t -> string -> Ident.t list -> Lambda.lambda -> Lam_stats.meta


