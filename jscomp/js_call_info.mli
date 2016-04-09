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



(** Type for collecting call site information, used in JS IR *) 

type arity = 
  | Full 
  | NA 


type call_info = 
  | Call_ml (* called by plain ocaml expression *)
  | Call_builtin_runtime (* built-in externals *)
  | Call_na 
  (* either from [@@js.call] or not available, 
     such calls does not follow such rules
     {[ fun x y -> f x y === f ]} when [f] is an atom     
  *) 


type t = { 
  call_info : call_info;
  arity : arity;

}

val dummy : t
val builtin_runtime_call : t

val ml_full_call : t 
