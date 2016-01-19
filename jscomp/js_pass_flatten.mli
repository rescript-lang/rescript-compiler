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



(** A pass converting nested js statement into a flatten visual appearance 

    Note this module is used to convert some nested expressions to flat statements, 
    in general, it's more human readable, and since it generate flat statements, we can spot
    some inline opportunities for the produced statemetns, 
    (inline) expressions inside a nested expression would generate ugly code.

    Since we are aiming to flatten expressions, we should avoid some smart constructors in {!J_helper}, 
    it  tries to spit out expression istead of statements if it can
*)

val program : J.program -> J.program
