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



(** Define intemediate format to be serialized for cross module optimization
 *)

(** In this module, 
    currently only arity information is  exported, 

    Short term: constant literals are also exported 

    Long term:
    Benefit? since Google Closure Compiler already did such huge amount of work
    TODO: simple expression, literal small function  can be stored, 
    but what would happen if small function captures other environment
    for example 

    {[
      let f  = fun x -> g x 
    ]}

    {[
      let f = g 
    ]}
*)

type cmj_value = {
  arity : Lam_stats.function_arities ;
  closed_lambda : Lambda.lambda option ; 
  (* Either constant or closed functor *)
}

type effect = string option

type cmj_table = {
  values : cmj_value String_map.t;
  pure : effect
}

val dummy : ?pure:string option -> unit -> cmj_table
