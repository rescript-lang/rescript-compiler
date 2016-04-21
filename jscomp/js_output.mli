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



(** The intemediate output when compiling lambda into JS IR *)

(* Hongbo Should we rename this module js_of_lambda since it looks like it's 
   containing that step
 *)

type st = Lam_compile_defs.st 

type finished = 
  | True 
  | False 
  | Dummy (* Have no idea, so that when [++] is applied, always use the other *)

type t  =  { 
  block : J.block ;
  value : J.expression option;
  finished : finished
}

val make : ?value: J.expression -> ?finished:finished -> J.block -> t

val of_stmt : ?value: J.expression -> ?finished:finished -> J.statement -> t

val of_block : ?value:J.expression -> ?finished:finished -> J.block -> t

val to_block : t -> J.block

val to_break_block : t -> J.block * bool 

module Ops : sig 
  val (++) : t -> t -> t 
end

val dummy : t 


val handle_name_tail :
    Lam_compile_defs.st ->
    Lam_compile_defs.return_type ->
    Lambda.lambda ->  J.expression -> t

val handle_block_return : 
    Lam_compile_defs.st ->
    Lam_compile_defs.return_type ->
    Lambda.lambda ->
    J.block -> J.expression -> t

val concat : t list -> t

val to_string : t -> string
